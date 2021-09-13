# Function to get the average variable importance from a k-fold ranger RF run on permuted train/test sets
# 
# @param .cv_rf_results_on_permuted_dfs: a list of length n_permutations with each item containing the k-fold model accuracy and
# the k-fold variable importances 
#
# @return Returns a dataframe of dimension p_variable x n_permutations columns
#                   var     perm1    perm2    perm3    perm4     perm5     perm6    perm7     perm8    perm9    perm10
# 1                 clump 24.702682 25.09524 24.88590 24.90882 24.450632 24.591842 23.88289 24.502652 23.03400 23.996852
# 2  uniformity_cell_size 15.838846 15.66576 15.79352 15.39607 16.655814 16.525283 16.78241 15.380106 14.88434 16.416847
# 3 uniformity_cell_shape 18.929354 18.67543 19.02305 19.29555 17.907899 17.214319 18.66861 19.278654 18.44553 19.886737
# 4              adhesion 17.693309 17.47849 20.28959 18.58819 18.675763 17.311907 17.74454 17.068503 17.04453 18.460552
# 5  epithelial_cell_size 18.344450 18.31556 17.86945 18.84495 19.181344 18.422843 18.33558 17.832939 20.57638 19.546999
# 6           bare_nuclei 16.641158 16.97925 17.30748 18.12716 17.818827 16.770799 17.64653 17.129329 19.96957 16.744783
# 7             chromatin 20.096892 23.16250 21.04735 19.68011 20.544083 20.393787 20.52757 21.176269 20.68509 20.501010
# 8              nucleoli 16.987984 16.05999 16.59370 15.05134 15.537712 16.475795 16.41029 15.620496 15.18527 16.439340
# 9               mitoses  9.731401 10.27956 10.63453 11.54957  9.302103  9.222525  9.93007  9.568537 10.24485  9.995452


get_dataframe_of_permuted_variable_importances <- function(.cv_rf_results_on_permuted_dfs){
  
  # Extract the variable importances from the list of permuted RF results
  random_var_importances <- map(cv_rf_results_on_permuted_dfs, "var_importances")
  
  # Get the average feature importance from the k-folds CV 
  random_var_importances_kfolds_averaged <- map(random_var_importances, 
                                                calculate_feature_mean_sd_importance_from_kfolds) 
  random_var_importances_kfolds_averaged_subset <- lapply(random_var_importances_kfolds_averaged, 
                                                          function(x) x %>% select(var, gini_index_mean))
  
  
  # Rename 'gini_index_mean' column with the permutation number
  col_permutation_names <- paste0("perm",seq(1,args$n_permutations))
  
  # Create the function
  change_colname <- function(df, new_colname){
    df = df %>% rename(!!new_colname := gini_index_mean)
  }
  
  random_var_importances_kfolds_averaged_subset_renamed <- map2(.x = random_var_importances_kfolds_averaged_subset, 
                                                                .y = col_permutation_names, 
                                                                ~change_colname(.x, .y))
  
  return(random_var_importances_kfolds_averaged_subset_renamed %>% 
           reduce(inner_join, by = "var"))
  
}