#' Set all elements of a vector or matrix above a certain value to that value
#'
#' @export
#' @param x A numeric vector or matrix.
#' @param at Value at which to cap \code{x}.
#' @return \code{x} but with values greater than \code{at} set equal to \code{at}
#' @examples
#' cap(1:5, at = 2)
cap <- function(x, at) {
  stopifnot(is.numeric(x), is.numeric(at))
  x[x > at] <- at
  x
}
