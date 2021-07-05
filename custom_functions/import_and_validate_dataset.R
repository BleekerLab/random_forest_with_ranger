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
  # This character column will be transformed into a factor for RF classification
  col_to_test <- colnames(df)[1]
  assertthat::assert_that(is.character(col_to_test))
  
  # convert first column into a factor
  df[,1] <- as.factor(df[,1])
  
  return(df)
  
}
