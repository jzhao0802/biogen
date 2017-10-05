create_date_diffs <- function(input, index_col = "index_date") {
  #Please input only a dataframe full of dates into this function:
  #input is the data frame
  date_cols <- input[, -which(colnames(input) == index_col)]
  
  date_diffs <- as.data.frame(sapply(date_cols, function(x) { 
    x - input[[index_col]]
    
  }))
  
  return(date_diffs)
}
