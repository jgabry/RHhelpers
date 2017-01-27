#' Wrapper for \code{cut()} with \code{labels = FALSE}, \code{right = FALSE}
#'
#' Integer codes are returned instead of a factor. Intervals closed on left and
#' open on right.
#'
#' @export
#' @param x,breaks same as \code{\link[base]{cut}}.
#' @return An integer vector of level codes. As with \code{cut}, values which
#'   fall outside the range of breaks are coded as NA (as are NaN and NA
#'   values).
#' @seealso \code{\link[base]{cut}}
#'
#' @examples
#' x <- 1:10
#' y <- cut_numeric_leftclosed(x, breaks = c(0, 2, 7))
#' cbind(x, y)
#'
#' z <- cut_numeric_leftclosed(x, breaks = c(1, 2, 7, Inf))
#' cbind(x, z)
#'
cut_numeric_leftclosed <- function(x, breaks) {
  cut(x, breaks = breaks, labels = FALSE, right = FALSE)
}
