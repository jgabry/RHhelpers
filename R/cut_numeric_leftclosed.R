#' Wrapper for \code{cut()} with \code{labels = FALSE}, \code{right = FALSE}
#' 
#' Integer codes are returned instead of a factor. Intervals closed on left and 
#' open on right.
#' 
#' @export 
#' @param x,breaks same as \code{\link[base]{cut}}.
#' @return an integer vector of level codes. 
#' @seealso \code{\link[base]{cut}}
#' 
cut_numeric_leftclosed <- function(x, breaks) {
  cut(x, breaks = breaks, labels = FALSE, right = FALSE)
}