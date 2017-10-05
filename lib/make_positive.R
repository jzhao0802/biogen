make_positive <- function(x) {
  x[x< 0] = NA
  return(x)
}
