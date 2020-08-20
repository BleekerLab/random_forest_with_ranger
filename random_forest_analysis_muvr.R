#!/usr/bin/env Rscript

##################
# Section 0: Setup
##################

###############
# 0.1 Libraries
###############
suppressPackageStartupMessages(library("MUVR"))
suppressPackageStartupMessages(library("tidyverse"))
suppressPackageStartupMessages(library("doParallel"))
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("doParallel"))


####################################
# 0.2 Parsing command-line arguments
####################################

option_list = list(
  make_option(c("-i", "--input_file"), 
              type = "character", 
              default = "data/housing.csv", 
              help="Path to input file (n columns for X features, 1 column for response variable), Separation will be infered from file extension (.csv: comma, .tsv: tabulation)", 
              metavar="filename"),
  make_option(c("-o", "--outdir"), 
              type="character", 
              default="rf_results", 
              help="output directory where to store results [default= %default]", 
              metavar="character"),
  make_option(c("-n", "--n_reps"), 
              type = "integer", 
              default = 5, 
              help="Number of repetitions to perform (set to >= 100 for real runs) [default= %default]",
              metavar="integer"),
  make_option(opt_str = "--n_outer", 
              type = "integer", 
              default=7, 
              help="Number of outer test segments to perform [default= %default]",
              metavar="integer"),
  make_option(opt_str = "--n_inner", 
              type = "integer", 
              default = 4, 
              help="Number of inner test segments to perform [default= %default]",
              metavar="integer"),
  make_option(c("-m", "--model"), 
              type="character", 
              default = "min", 
              help="Model choice ('min', 'mid' or 'max') [default= %default]"),
  make_option(opt_str = c("-b", "--best_params"), 
              action = "store_true",
              type="logical", 
              default = FALSE,
              help="Whether to search for the best variable ratio parameters: TRUE if option is set [default= %default]."),
  make_option(opt_str = "--variable_ratio", 
              type="double", 
              default=0.6, 
              help="(if --best_params flag selected) Ratio of variables used for feature selection [default= %default]"),
  make_option(opt_str = "--start_variable_ratio", 
              type="double", 
              default=0.6, 
              help="(if --best_params flag selected) Starting point for best ratio of variables search [default= %default]"),
  make_option(opt_str = "--stop_variable_ratio", 
              type="double", 
              default=0.9, 
              help="(if --best_params flag selected) End point for best ratio of variables search [default= %default]"),
  make_option(opt_str = "--step_variable_ratio", 
              type="double", 
              default=0.1, 
              help="(if --best_params flag selected) Step size for best ratio of variables search [default= %default]"),
  make_option(c("-k", "--n_permutations"), 
              type = "integer", 
              default=100,
              metavar = "integer",
              help="Number of permutations [default= %default]"),
  make_option(c("-p", "--n_cores"), 
              type="integer", 
              default = 1, 
              help="Number of cores/CPUs to use (parallel execution) [default= %default]",
              metavar="integer")
) 
opt_parser = OptionParser(option_list=option_list,
                          description = "\n A program to perform a Random Forest analysis based on the MUVR R package ",
                          epilogue = "Please visit https://cran.r-project.org/web//packages/MVR/MVR.pdf and https://github.com/BleekerLab/random_forest_with_muvr for additional information");
args = parse_args(opt_parser)

########################
# 0.3 Cluster generation
########################
cl = makeCluster(args$n_cores)   
registerDoParallel(cl)


##################################
# Section 1: reading input dataset
##################################

# check for file extension
if (grepl("\\.csv$", args$input_file)) {
  df = read.csv(args$input_file,
                  header = T,
                  stringsAsFactors = F,
                  check.names = F)
} else if (grepl("\\.tsv$", args$input_file)) {
  df = read.delim(args$input_file,
                header = T,
                stringsAsFactors = F,
                check.names = F)
} else {
  stop("Please make sure your file is either: \n
        comma-separated and ends with .csv or \n
        tab-separated and ends with .tsv)")
}

n_cols_features = ncol(df) - 1 # all columns should contain features but the last one.

X = df[,1:n_cols_features]
y = df[,ncol(df)]

cat("\n#####################################################################\n")
cat("\n Section 1: reading file successfully executed!                      \n")
cat("\n",nrow(df), "observations are present in your dataset.               \n")
cat("\n",ncol(df) - 1, "features will be considered.                        \n")
cat("\n The column named ",colnames(df)[ncol(df)], "will be used as your Y  \n")
cat("\n#####################################################################\n")


###############################################
# Section 2: Optional search for best parameters
###############################################
if (args$best_params == TRUE){
  
  # grid search params
  start_ratio <- args$start_variable_ratio
  end_ratio <-   args$stop_variable_ratio
  step_ratio <-  args$step_variable_ratio
  
  hyper_grid <- expand.grid(
    ratio     = seq(from = start_ratio, 
                    to   = end_ratio, 
                    by   = step_ratio),
    q2     = 0 # will be used to collect the Q2 metric for each RF model
  )
  
  for (i in 1:nrow(hyper_grid)){
    print(paste0("testing parameter combination ", i, " out of ", nrow(hyper_grid)))
    
    rf_model <- MUVR(X =        X, 
                     Y =        y,
                     ID =       IDs,
                     nRep =     args$n_reps,
                     nOuter =   args$n_outer,
                     nInner =   args$n_inner,
                     varRatio = hyper_grid$ratio[i],
                     scale =    FALSE, 
                     DA =       FALSE, 
                     fitness =  "RMSEP", 
                     method =   "RF", 
                     parallel = TRUE)
    
    if (args$model == "min"){
      hyper_grid$q2[i] <- rf_model$fitMetric$Q2[1] # min model
    } else if (args$model == "mid") {
      hyper_grid$q2[i] <- rf_model$fitMetric$Q2[2] # mid model
    } else if (args$model == "max") {
      hyper_grid$q2[i] <- rf_model$fitMetric$Q2[3] # max model
    } else {
      stop("please make sure you choose either 'min', 'mid' or 'max' as model")
    }
  }
  
  # extract the best parameters
  best_params <- hyper_grid[which.max(hyper_grid$q2),]

  # plot evolution of Q2 against parameters
  optimization_plot <- hyper_grid %>% 
    mutate(ratio = as.factor(ratio)) %>% 
    rownames_to_column("combination") %>% 
    mutate(combination = as.numeric(combination)) %>% 
    ggplot(., aes(combination, y = q2)) + 
    geom_point() + 
    scale_y_continuous(limits = c(0,1))
  
} else {
  cat("\n#####################################################################\n")
  cat("\nSection 2: searching for best parameters skipped.                    \n")
  cat("\nPlease add the --best_params flag if you'd like to perform this step.\n")
  cat("\nUsing ", args$variable_ratio," as variable ratio                     \n")
  cat("\n#####################################################################\n")
}


##############################################################################
# Section 3: Random Forest analysis *with* best parameters on original dataset
##############################################################################

if (args$best_params == TRUE){
  # using best params
  rf_model <- MUVR(X =        X, 
                   Y =        y,
                   nRep =     args$n_reps,
                   nOuter =   args$n_outer,
                   nInner =   args$n_inner,
                   varRatio = best_params$ratio,
                   scale =    FALSE, 
                   DA =       FALSE, 
                   fitness =  "RMSEP", 
                   method =   "RF", 
                   parallel = TRUE)
} else {
  rf_model <- MUVR(X =        X, 
                   Y =        y,
                   nRep =     args$n_reps,
                   nOuter =   args$n_outer,
                   nInner =   args$n_inner,
                   varRatio = args$variable_ratio,
                   scale =    FALSE, 
                   DA =       FALSE, 
                   fitness =  "RMSEP", 
                   method =   "RF", 
                   parallel = TRUE)
}

if (args$model == "min"){
  model_original_q2 <- rf_model$fitMetric$Q2[1] # min model
} else if (args$model == "mid") {
  model_original_q2 <- rf_model$fitMetric$Q2[2] # mid model
} else if (args$model == "max") {
  model_original_q2 <- rf_model$fitMetric$Q2[3] # max model
} 

cat("\n#####################################################################\n")
cat("\nSection 3: Random Forest analysis on original dataset completed.     \n")
cat("\n RF model Q2 is equal to: ", model_original_q2,                     "\n")
cat("\n#####################################################################\n")

#################################
# Section 4: Permutation analysis
#################################

###############################
# 4.1: permutations 
# compute permuted models 
# collect Q2 from permutations
##############################
n_permutations <- args$n_permutations
perm_fit = numeric(n_permutations)

# computed permuted models
# collect permuted feature importances
features_permuted_pvalues_matrix <- matrix(0, 
                                           nrow = ncol(X), # One row = one feature
                                           ncol = n_permutations)

rownames(features_permuted_pvalues_matrix) = colnames(X)
colnames(features_permuted_pvalues_matrix) = paste0("permutation",seq(1:n_permutations))


if (args$best_params == TRUE){
  var_ratio = best_params$ratio
} else {
  var_ratio = args$variable_ratio
}

# permutations
cat("\nStarting permutations")
for (p in 1:n_permutations) {
  cat('\nPermutation',p,'of', n_permutations)
  YPerm = sample(y)
  perm = MUVR(X         = X, 
              Y         = YPerm,
              nRep      = args$n_reps,
              nInner    = args$n_inner,
              nOuter    = args$n_outer,
              varRatio  = var_ratio,
              scale     = FALSE, 
              DA        = FALSE, 
              fitness   = "RMSEP", 
              method    = "RF", 
              parallel  = TRUE)
  # for model
  perm_fit[p] = perm$fitMetric$Q2
  # for each variable
  features_permuted_pvalues_matrix[,p] = as.vector(perm$VIP[,"min"])
}

###########################################################
### 4.2: plot of RF model (actual fit vs permuted fits) ###
###########################################################

# actual (original RF model)
if (args$model == "min"){
  actual_fit <- rf_model$fitMetric$Q2[1] # min model
} else if (args$model == "mid") {
  actual_fit <- rf_model$fitMetric$Q2[2] # mid model
} else if (args$model == "max") {
  actual_fit <- rf_model$fitMetric$Q2[3] # max model
} 

# Parametric (Student’s) permutation test significance
pvalue <- pPerm(actual = actual_fit, 
                h0 = perm_fit,
                side = "greater",
                type = "t")

plotPerm(actual = actual_fit, 
         xlab = "Q2 metric",
         h0 = perm_fit) 


perm_fit_df = data.frame(permutation = seq(1:length(perm_fit)), 
                         q2 = perm_fit) 

model_permutation_plot <- ggplot(perm_fit_df, aes(x = q2)) + 
  geom_histogram(bins = 10) + 
  geom_vline(xintercept = actual_fit, col = "blue") + 
  labs(x = "Q2 metric", y = "Frequency") +
  ggtitle(paste("Distribution of Q2 p-values based on \n",
                n_permutations,
                "permutations of the Y variable \n p-value = ",
                format(pvalue,digits = 3, decimal.mark = "."),sep = " "
                ))

###########################################################
### 4.3: extract significant p-values for each variable ###
###########################################################


cat("\n###########################################\n")
cat("\nSection 4: Permutation analysis completed. \n")
cat("\n###########################################\n")

#########################
# Section 5: save results
#########################
if (args$best_params == TRUE){
  var_ratio = best_params$ratio
} else {
  var_ratio = args$variable_ratio
}

params_df <- data.frame(
    n_cores =        args$n_cores,
    n_outer =        args$n_outer,
    n_inner =        args$n_inner,
    var_ratio =      var_ratio,
    n_permutations = args$n_permutations,
    model =          args$model)

save(df,
     X,
     y,
     rf_model,
     hyper_grid,
     params_df,
     optimization_plot,
     model_permutation_plot,
     file = "rf_analysis.RData",
     compress = "gzip",
     compression_level = 6)



stopCluster(cl)