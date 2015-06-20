#' Normalize to have mean 1 
#' 
#' @export 
#' @param x vector to normalize to have mean 1. 
#' @return \code{x / mean(x)}
#' 
normalize <- function(x) {
  # normalize to have mean 1
  stopifnot(is.numeric(x))
  x / mean(x)
}