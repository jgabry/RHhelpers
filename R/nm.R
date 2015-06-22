#' Get object name as string
#' @export
#' @param x an object
#' @return The name of \code{x} as a character string.
nm <- function(x) {
  deparse(substitute(x))
}
