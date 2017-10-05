remove_na <- function(x) {
  x = x[! is.na(x)] 
  return(x)
}