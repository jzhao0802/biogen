mystat <- function(x) {
  min_ = min(unlist(x), na.rm=TRUE)
  max_ = max(unlist(x), na.rm=TRUE)
  return(c(min=min_,max=max_))
}  