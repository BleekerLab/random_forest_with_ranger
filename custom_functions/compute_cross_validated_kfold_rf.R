# Function to run one ranger RF analysis
# The number of k-folds is determined by the length of the train_test_set list
#
# @param .list_of_train_test_sets: a list that contains split train/test sets. 1 train/test pair per list item
# @param .num_trees: number of tree estimators to be created
# @param .importance: criteria for variable importance measurements. "impurity" is the Gini index
#
# @return Returns the model k-fold accuracies and k-fold importances per variable

compute_kfold_cv_rf <- function(list_of_train_test_sets, 
                                .num_trees = 5000, 
                                .importance = "impurity",
                                .num.threads = 1){
  
  # extract train/test pairs
  train_sets <- map(list_of_train_test_sets, 1)
  test_sets <- map(list_of_train_test_sets, 2)
  
  # Build k_folds RF models using ranger
  ranger_models <- map(.x = train_sets, 
                       .f = function(x) ranger(data = x,
                                               dependent.variable.name = colnames(x)[1], 
                                               num.trees = .num_trees, 
                                               importance = .importance )
                       )
  

  # for each test set and ranger model, compute the model accuracy 
  # returns a vector of model accuracies e.g. c(99.28571, 96.42857, 95.71429, 98.57143, 97.14286)
  models_accuracy <- map2_dbl(.x = test_sets, 
                          .y = ranger_models, 
                          .f = function(x, y) get_ranger_model_accuracy(.test_set = x, .ranger_model = y))
  
  # extract variable importances
  # each variable gets k_folds variable importance estimates
  var_importances <- map_dfr(.x = ranger_models, "variable.importance")
  
  return(
    list("accuracy" = models_accuracy, 
         "var_importances" = var_importances)
    )
}
