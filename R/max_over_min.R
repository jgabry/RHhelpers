#' Ratio of maximum to minimum
#' 
#' @export 
#' @param x numeric vector.
#' @return \code{max(x) / min(x)}
#' 
max_over_min <- function(x) {
  max(x) / min(x)
}