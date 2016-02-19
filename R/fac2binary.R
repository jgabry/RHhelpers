#' Convert 2-level factor variable to a binary 0/1 variable
#'
#' @export
#' @param x A 2-level factor variable
#' @return \code{x}, converted to a binary variable
#'
fac2binary <- function(x) {
  stopifnot(is.factor(x), length(levels(x)) == 2L)
  y <- as.numeric(x)
  stopifnot(all(na.omit(y) %in% c(1,2)))
  plyr::mapvalues(y, from = 1:2, to = 0:1)
}
