# Creates a list of permuted dataframes 
# This is useful to compute a distribution of random Random Forest model accuracies based on randomly permuted dfs
#
# @param .df:             the original dataframe with the first column containing the sample class labels
# @param .initial_seed:   the initial number for the pseudorandom seed
# @param .n_permutations: the number of permutations to perform. This will be the length of the final list.
#
# @return Returns a list of permuted dataframes with the length of the list equal to n_permutations
create_list_of_permuted_dataframes <- function(.df, 
                                               .initial_seed, 
                                               .n_permutations){
  seeds <- map_dbl(.x = seq_along(1:.n_permutations), 
                   .f = function(x){.initial_seed + x})
  
  list_of_permuted_dataframes <- map(.x = seeds,           
                                      function(x)(permute_dataframe_on_y_column(.df, myseed = x))
                                     )
  
  return(list_of_permuted_dataframes)
}
