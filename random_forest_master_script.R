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
suppressPackageStartupMessages(library("assertr"))
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
              help="Initial seed used for the analysis [default= %default]")
) 
opt_parser = OptionParser(option_list=option_list,
                          description = "\n A program to perform a Random Forest analysis based on the ranger R package ",
                          epilogue = "Please visit https://cran.r-project.org/web//packages/ranger/ranger.pdf and https://github.com/imbs-hl/ranger for additional information");
args = parse_args(opt_parser)


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
cv_rf_results <- compute_kfold_cv_rf(list_of_train_test_sets = train_test_sets, .num_trees = args$n_trees)


###################################################################################
# Section 6: plot mean/sd model accuracy versus distribution of permuted accuracies 
###################################################################################


###################################################################################
# Section 6: plot mean/sd model accuracy versus distribution of permuted accuracies 
###################################################################################


# Step 1: crunch numbers = calculate original CV mean/sd model accuracy before plotting
# Step 2: crunch numbers = calculate original mean/sd variable importance before plotting