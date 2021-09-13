# Function to get the average and standard deviation variable importance from a k-fold ranger RF run on train/test sets
# 
# @param .var_imp_from_kfold_rf: a dataframe with k-fold rows x p_variable columns with the variable importance
#
# @return Returns a dataframe of dimension p_variable x 3 columns (variable name, mean, sd) 
# 
#                    var gini_index_mean gini_index_sd
#                 clump       14.709803     1.8701575
#  uniformity_cell_size       68.405725     2.7745320
# uniformity_cell_shape       57.952563     1.9699513

calculate_feature_mean_sd_importance_from_kfolds <- function(.var_imp_from_kfold_rf){
  
  # Calculate feature mean Gini index variable importance
  feature_means <- colMeans(.var_imp_from_kfold_rf) %>% 
    as.data.frame() %>% 
    rename("gini_index_mean" = ".") %>% 
    rownames_to_column("var")
  # Calculate feature standard deviation Gini index variable importance
  feature_sds = .var_imp_from_kfold_rf %>% 
    summarise_if(is.numeric, sd) %>% 
    t() %>% 
    as.data.frame() %>% 
    rename("gini_index_sd" = "V1") %>% 
    rownames_to_column("var")
  
  feature_means_and_sds <- inner_join(feature_means, 
                                      feature_sds)
  return(feature_means_and_sds)
}