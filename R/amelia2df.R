#' Retrieve an imputed dataset (as a data frame) from an amelia object
#'
#' @export
#' @param x An object of class \code{"amelia"} obtained from running
#' the \code{\link[Amelia]{amelia}} function.
#' @param which_imp If \code{x} contains multiple imputed datasets (i.e., if
#'   \code{m} argument to \code{amelia} was greater than 1), which of the
#'   imputed datasets should be returned?
#'
#' @return A data.frame
#'
amelia2df <- function(x, which_imp = 1) {
  stopifnot(inherits(x, "amelia"), is.numeric(which_imp))
  imps <- x$imputations[[which_imp]]
  as.data.frame(imps)
}
