#!/usr/bin/env Rscript

##################
# Section 0: Setup
##################

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
suppressPackageStartupMessages(library("optparse"))


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
  make_option(c("-k", "--n_permutations"), 
              type = "integer", 
              default=10,
              metavar = "integer",
              help="Number of permutations (Usually > 100 and up to 1000) [default= %default]"),
  make_option(c("-p", "--n_cores"), 
              type="integer", 
              default = 1, 
              help="Number of cores/CPUs to use (parallel execution) [default= %default]",
              metavar="integer")
) 
opt_parser = OptionParser(option_list=option_list,
                          description = "\n A program to perform a Random Forest analysis based on the ranger R package ",
                          epilogue = "Please visit https://cran.r-project.org/web//packages/ranger/ranger.pdf and https://github.com/imbs-hl/ranger for additional information");
args = parse_args(opt_parser)


##################################
# Section 1: reading input dataset
##################################

# check for file extension
if (grepl("\\.csv$", args$input_file)) {
  df = read.csv(args$input_file,
                  header = T,
                  stringsAsFactors = F,
                  check.names = F)
} else {
  stop("Please make sure your file is comma-separated and ends with .csv")
}

# Separate variables and sample class
n_cols_features = ncol(df) - 1 # all columns should contain features but the last one.
X = df[,2:n_cols_features]     # variables
y = df[,1]                     # sample class

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
  
  cat("\n#####################################################################\n")
  cat("\nSection 2: searching for best parameters done!                       \n")
  cat("\nUsing ", best_params$ratio," as variable ratio                       \n")
  cat("\n#####################################################################\n")
  
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
  if (args$model == "min"){
    perm_fit[p] <- perm$fitMetric$Q2[1] # min model
  } else if (args$model == "mid") {
    perm_fit[p] <- perm$fitMetric$Q2[2] # mid model
  } else if (args$model == "max") {
    perm_fit[p] <- perm$fitMetric$Q2[3] # max model
  } 

  # for each variable
  features_permuted_pvalues_matrix[,p] = as.vector(perm$VIP[,args$model])
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


perm_fit_df = data.frame(
  permutation = seq(1:length(perm_fit)), 
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
m <- t(features_permuted_pvalues_matrix)
original_vips <- rf_model$VIP[,args$model]
p_values <- vector(mode = "numeric", length = ncol(m))

#Count how many times the original VIP was inferior to permuted VIPs
for (i in seq_along(1:ncol(m))) {
  permuted_vips = as.vector(m[,i])
  pvalue = sum(permuted_vips < original_vips[i])/n_permutations
  p_values[i] = pvalue
}

# Values equal to 0 are impossible so replace with inferior to threshold 1/N permutations
p_values[p_values == 0] <- paste("p <",round(1/n_permutations, digits = 3))

# Create final dataframe containing variables + p-values
features_pvalues_df =
  features_permuted_pvalues_matrix %>%
  as.data.frame() %>%
  rownames_to_column("feature") %>%
  mutate(original = original_vips, p_value = p_values)
  
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
    n_reps  =        args$n_reps,
    n_outer =        args$n_outer,
    n_inner =        args$n_inner,
    var_ratio =      var_ratio,
    n_permutations = args$n_permutations,
    model =          args$model)


dir.create(path = args$outdir, showWarnings = FALSE, recursive = TRUE)

if (args$best_params == TRUE){
  save(df,
       X,
       y,
       rf_model,
       params_df,
       hyper_grid,         # --best_params flag "on"
       optimization_plot,  # --best_params flag "on"
       model_permutation_plot,
       features_pvalues_df,
       file = file.path(args$outdir, "rf_analysis.RData"),
       compress = "gzip",
       compression_level = 6)
} else {
  save(df,
       X,
       y,
       rf_model,
       params_df,
       model_permutation_plot,
       features_pvalues_df,
       file = file.path(args$outdir, "rf_analysis.RData"),
       compress = "gzip",
       compression_level = 6)
}

cat("\n############################################################################\n")
cat("\nSection 5: Results saved to", file.path(args$outdir, "rf_analysis.RData"), "\n")
cat("\n############################################################################\n")


stopCluster(cl)
