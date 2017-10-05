make_negative <- function(x) {
  x[x> 0] = NA
  return(x)
}
