remove_inf <- function(x) {
  x[is.infinite(x)] = NA
  return(x)
}
