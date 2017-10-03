cut_linux <- function(charlist, field=1, delimiter="_") {
  ( unlist(lapply(charlist, function(x) { x_ = strsplit(x, delimiter); (x_[[1]][field]) }) ) )
}
