# Permute a dataframe on the class (y) column
# This is useful to compute a distribution of Random Forest model accuracies based on randomly permuted dfs
#
# @param df:     a dataframe with the first column containing the sample class labels
# @param .myseed: the initial number for the pseudorandom seed
#
# @return Returns a dataframe with the first column permuted (the first column should be a factor)
permute_dataframe_on_y_column <- function(df, 
                                          .myseed = initial_seed){
  # myseed has to be an integer e.g. 123
  assert_that(is.numeric(.myseed))

  # extract original y vector of classes
  y <- df[,1] 
  y_col_name <- colnames(df)[1]
  
  # randomly permute based on the choosen seed
  set.seed(.myseed)
  permuted_y <- permute(y)
  
  # replace original column with permuted values
  permuted_df <- df %>% 
    dplyr::select(-!!y_col_name) %>% # remove original column
    dplyr::mutate(!!y_col_name := permuted_y) %>% 
    dplyr::relocate(y_col_name)
  
  return(permuted_df)
}
