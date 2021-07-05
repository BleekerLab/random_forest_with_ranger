# Permute a dataframe on the class (y) column
# This is useful to compute a distribution of Random Forest model accuracies based on randomly permuted dfs
#
# @param df a dataframe with the first column containing the sample class labels
permute_dataframe_on_y_column <- function(df, 
                                          myseed = initial_seed){

  # extract original y vector of classes
  y <- df[,1] 
  
  # randomly permute based on the choosen seed
  set.seed(myseed)
  permuted_y <- permute(y)
  
  # replace original column with permuted values
  permuted_df <- df %>% 
    dplyr::select(-!!y_column) %>% # remove original column
    dplyr::mutate(!!y_column := permuted_y)
  
  return(permuted_df)
}