# Function to create a list of train and test datasets
# The length of the list is equal to the number of k_folds

# @param df:       Dataframe with sample class in the first column and variables in the other columns
# @param myseed:   The initial seed value used for the analysis 
# @param .k_folds: The number of splits during the cross-validation procedure 
#
# @return Returns a list (length = k_folds) of test and train datasets 
create_kfold_train_test_sets <- function(mydata = df, 
                                         myseed = initial_seed, 
                                         .k_folds = 5){
  # creates a list of seeds for the k_fold splits
  seeds <- map_dbl(.x = seq_along(1:.k_folds), 
                   .f = function(x){myseed + x})
  
  # creates a list of train/test dataset
  # 1 pair train/test per list element
  list_of_train_test_sets <- map(.x = seeds, 
                                 .f = function(x) create_train_test_sets(mydata, myseed = x))
  
  # the final list has length = .k_folds
  return(list_of_train_test_sets)
}
