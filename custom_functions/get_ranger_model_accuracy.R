# Function to calculate one random forest model accuracy

# @param test_set:     a dataset from which sample class will be predicted
# @param ranger_model: an object of class ranger that will be used to predict sample class from the test set. 
#
# @return Returns the ranger model accuracy  

get_ranger_model_accuracy <- function(.test_set, .ranger_model){
  # get sample class based on their variable measurements
  predicted_sample_classes <- predict(.ranger_model, .test_set)
  
  # count success = good sample classification
  number_of_success <- 
    data.frame(predicted = predicted_sample_classes$predictions,
               truth = .test_set[,1]) %>% 
    mutate("success" = predicted == truth) %>% 
    with(., sum(success)) # count the number of TRUE
  
  # model accuracy = number of samples / number of good classifications
  model_accuracy <- number_of_success / nrow(.test_set) * 100
  
  return(model_accuracy)
}
