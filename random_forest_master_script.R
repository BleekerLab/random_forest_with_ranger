#!/usr/bin/env Rscript

#####################################################
# Section 0: Setup: libraries, command-line arguments
#####################################################

if ("checkpoint" %in% installed.packages()){
  library("checkpoint")
  checkpoint("2021-01-01")
} else {
  install.packages("checkpoint")
  library("checkpoint")
  checkpoint("2021-01-01")
}

###############
# 0.1 Libraries
###############
suppressPackageStartupMessages(library("ranger"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("assertthat"))
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("rlang"))
suppressPackageStartupMessages(library("gtools")) 


######################
# 0.2 Custom functions
######################

source("custom_functions/import_and_validate_dataset.R")
source("custom_functions/create_train_test_set.R")
source("custom_functions/create_list_of_train_test_sets.R")
source("custom_functions/compute_cross_validated_kfold_rf.R")
source("custom_functions/get_ranger_model_accuracy.R")
source("custom_functions/create_list_of_permuted_dataframes.R")
source("custom_functions/permute_dataframe.R")
source("custom_functions/get_accuracies_from_permuted_cv_rf_results.R")
source("custom_functions/calculate_feature_mean_sd_importance_from_kfolds.R")
source("custom_functions/get_dataframe_of_permuted_variable_importances.R")
source("custom_functions/calculate_feature_pvalue.R")

####################################
# 0.2 Parsing command-line arguments
####################################

option_list = list(
  make_option(c("-i", "--input_file"), 
              type = "character", 
              default = "data/breast-cancer.csv", 
              help="Path to .csv input file (n columns for X features, 1 column for sample class)", 
              metavar="filename"),
  make_option(c("-o", "--outdir"), 
              type="character", 
              default="rf_results", 
              help="output directory where to store results [default= %default]", 
              metavar="character"),
  make_option(c("-k", "--k_folds"), 
              type = "integer", 
              default = 5,
              metavar = "integer",
              help="Number of k-fold cross-validation to be performed (Usually between 5 and 10 folds) [default= %default]"),
  make_option(c("-n", "--n_permutations"), 
              type = "integer", 
              default = 10,
              metavar = "integer",
              help="Number of permutations (Usually > 100 and up to 1000) [default= %default]"),
  make_option(c("-t", "--n_trees"), 
              type = "integer", 
              default = 5000,
              metavar = "integer",
              help="Number of trees to be build in each individual random forest (Usually > 1000 and up to 10000) [default= %default]"),
  make_option(c("-s", "--initial_seed"), 
              type = "integer", 
              default = 123,
              metavar = "integer",
              help="Initial seed used for the analysis [default= %default]"),
  make_option(c("-p", "--threads"), 
              type = "integer", 
              default = 1,
              metavar = "integer",
              help="Number of threads to be used [default= %default]")
) 
opt_parser = OptionParser(option_list=option_list,
                          description = "\n A program to perform a Random Forest analysis based on the ranger R package ",
                          epilogue = "Please visit https://cran.r-project.org/web//packages/ranger/ranger.pdf and https://github.com/imbs-hl/ranger for additional information");
args = parse_args(opt_parser)

dir.create(args$outdir)

##################################
# Section 1: reading input dataset
##################################

df <- import_and_validate_dataset(file_path = args$input_file)

cat("\n#####################################################################\n")
cat("\n Section 1: reading file successfully executed!                      \n")
cat("\n",nrow(df), "observations are present in your dataset.               \n")
cat("\n",ncol(df) - 1, "features/variables will be considered.              \n")
cat("\n The column named",colnames(df)[1], "will be used as your Y          \n")
cat("\n#####################################################################\n")



#######################################################################################
# Section 2: Compute Random Forest k-fold cross-validation analysis on original dataset
#            Returns k_folds model accuracies
#            Returns k_folds variable importances
#######################################################################################

# pseudocode
# Step 1: create a list of k_fold train and test datasets
# Step 2a: run ranger RF on each of the f_fold train/test pair 
# Step 2b: calculate the model classification accuracy for each k_fold iteration
# Step 2c: retrieve the variable importances and create a dataframe

# Step 1: create a list of k_fold train and test datasets
train_test_sets <- create_kfold_train_test_sets(mydata = df, 
                                                myseed = args$initial_seed, 
                                                .k_folds = args$k_folds)

# Step 2a: run ranger RF on each of the f_fold train/test pair 
# Step 2b: calculate the model classification accuracy for each k_fold iteration
# Step 2c: retrieve the variable importances and create a dataframe
cv_rf_results <- compute_kfold_cv_rf(
  list_of_train_test_sets = train_test_sets,
  .num_trees = args$n_trees,
  .num.threads = args$threads,
  .importance = "impurity"
  )



###################################################################################################
# Section 3: create a series of dataframes and compute random model accuracies and var. importances 
# One random accuracy per permutation
# One variable importance per variable and per permutation
###################################################################################################

list_of_permuted_dfs <- create_list_of_permuted_dataframes(.df = df, 
                                                           .initial_seed = args$initial_seed, 
                                                           .n_permutations = args$n_permutations)


# creates a list of seeds for the n_permutations
seeds <- map_dbl(.x = seq_along(1:args$n_permutations), 
                   .f = function(x){args$initial_seed + x})


train_test_permuted_sets <- map2(.x = list_of_permuted_dfs, 
                                 .y = seeds, 
                                 .f = function(x,y) 
                                   create_kfold_train_test_sets(
                                                mydata = x, 
                                                myseed = y, 
                                                .k_folds = args$k_folds)
                                 )

cv_rf_results_on_permuted_dfs <- 
  map(.x = train_test_permuted_sets, 
      .f = function(x)(compute_kfold_cv_rf(x,
                                           .num_trees = args$n_trees, 
                                           .num.threads = args$threads)))

# Average random model accuracy
random_model_accuracies <- 
  tibble(acc = get_accuracies_from_permuted_cv_rf_results(cv_rf_results_on_permuted_dfs)) 
  
# Average variable importances



###################################################################################
# Section 4.: plot mean/sd model accuracy versus distribution of permuted accuracies 
###################################################################################

# calculate mean/sd of model accuracy from original cv rf results
# add a confidence interval of 95% for the mean
cv_rf_original_model_results <- tibble(mean_acc = mean(cv_rf_results$accuracy), 
                                       sd_acc = sd(cv_rf_results$accuracy)) %>% 
  # Calculate confidence interval
  # assumes that original model accuracies follow a normal distribution
  # source: https://www.statology.org/confidence-interval-in-r/
  mutate(margin = qt(0.95, df = args$k_folds) * sd_acc / sqrt(args$k_folds)) %>% 
  mutate(upper_conf_int = 
           ifelse(test = mean_acc + margin >= 100,
                  yes = 100,
                  no =  mean_acc + margin),
         lower_conf_int = ifelse(mean_acc - margin <= 0,yes = 0,no = mean_acc - margin)
         ) 


# Plot original model accuracy versus distribution of random accuracies
p_model_accuracy <- 
  ggplot(random_model_accuracies, aes(x = acc)) +
         geom_density() +
  geom_vline(data = cv_rf_original_model_results, 
             aes(xintercept = mean_acc, colour = "Average")) +
  geom_vline(data = cv_rf_original_model_results, 
             aes(xintercept = lower_conf_int, colour = "Lower")) +
  geom_vline(data = cv_rf_original_model_results, 
             aes(xintercept = upper_conf_int, colour = "Upper")) +
  scale_x_continuous(limits = c(0,100)) +
  scale_colour_manual(name = "Original model accuracy\n(95% confidence interval)", 
                      values = c(Average = "#1b9e77", Lower = "#d95f02", Upper = "#7570b3"))

p_model_accuracy
ggsave(filename = file.path(args$outdir,"model_accuracy.pdf"), plot = p_model_accuracy, width = 10, height = 7)

######################################################################################################
# Section 5: extract a table of significant features with their original mean and sd Gini index values
######################################################################################################

### Step 1: crunch numbers = calculate original CV mean/sd feature importance
original_var_importances <- calculate_feature_mean_sd_importance_from_kfolds(.var_imp_from_kfold_rf = cv_rf_results$var_importances)


### Step 2: crunch numbers = calculate permuted CV mean/sd feature importance for each permutation
# Calculate average and SD from the k-folds CV for each permutation
# First extract the variable importances from each permutation
# Then calcualte the mean/sd
# Finally select only the variable name and its mean gini index mean
permuted_var_importances <- get_dataframe_of_permuted_variable_importances(cv_rf_results_on_permuted_dfs)

## Step 3: calculate p-values
feature_pvalues <- calculate_feature_pvalue(original_var_importances, permuted_var_importances, .n_permutations = args$n_permutations)

### Step 4: write table with results
original_var_importances <- left_join(original_var_importances, feature_pvalues)

write.csv(x = original_var_importances, 
          file = file.path(args$outdir, "feature_gini_impurity_and_pvalues.csv"), 
          row.names = FALSE, 
          quote = FALSE)



