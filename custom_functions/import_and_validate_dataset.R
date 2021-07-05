# A function to import the .csv file containing the data and validating the values
# If successful, returns a `df` R object ready for Random Forest analysis

import_and_validate_dataset <- function(file_path){
  # check for file extension: has to be .csv and readable
  if (grepl("\\.csv$", args$input_file)) {
    df = read.csv(args$input_file,
                  header = T,
                  stringsAsFactors = F,
                  check.names = F)
  } else {
    stop("Please make sure your file is comma-separated and ends with .csv")
  }
  
  # The first column has to contain character values not numbers, etc. 
  # This in order to be transformed into a factor for RF
  col_to_test <- colnames(df)[1]
  assert(data = df, 
         predicate = is.character,       # has to return TRUE
         col_to_test,                    # the sample class column
         success_fun = success_logical)  # returns TRUE if assert is successful
  # convert sample class into a factor
  df[,1] <- as.factor(df[,1])
  
  return(df)
  
}