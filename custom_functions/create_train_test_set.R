# This function takes a dataframe and split it into a train and a test set
# Default values for the seed and the train/test ratio are given. 

# @param data: a dataframe with the sample class labels in the first column and the variables in the other columns. 
# @param myseed: the number to initialise the pseudorandom seed
# @param ratio: the train/test ratio e.g. 0.8 means that 80% of the samples will be used for training and 20% for testing
#
# @Result a list with the first element being the training set and the second being the test set

create_train_test_sets <- function(data, 
                                   myseed = 123, 
                                   ratio = 0.8){
  
  set.seed(myseed)
  
  sampled_rows_train <- sample(x = seq(1:nrow(data)), 
                               size = round(ratio*nrow(data),digits = 0)
  )
  train_set <- data[sampled_rows_train, ]
  test_set <- data[-sampled_rows_train, ]
  
  return(list("train_set" = train_set, 
              "test_set" = test_set))
}