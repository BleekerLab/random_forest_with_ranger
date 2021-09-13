# Function to get the accuracies from the ranger RF analysis on permuted train/test sets
#
# @param .cv_rf_results_on_permuted_dfs: a list that contains n_permutations items. Each item contains
# two lists: 
#  - The first nested element is a vector of accuracies (length = k-folds) from one permuted df
#  - The second nested element is a dataframe of k-folds x n variabels of the variable importances from one permuted df
#
# @return Returns a vector of length n_permutations with the averaged model accuracies
get_accuracies_from_permuted_cv_rf_results <- function(.cv_rf_results_on_permuted_dfs){

  # Extract accuracies and average them
  random_accuracies <- map(.cv_rf_results_on_permuted_dfs, "accuracy")
  random_accuracies_averaged <- map_dbl(random_accuracies, mean)
  # Results in a vector with length = n_permutations  
  return(random_accuracies_averaged)
}