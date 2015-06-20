#' Coefficient of variation
#' 
#' @export 
#' @param x numeric vector.
#' @return \code{sd(x) / mean(x)}
#' 
cv <- function(x) {
  stopifnot(is.numeric(x))
  sd(x) / mean(x)
}