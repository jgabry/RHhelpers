#' Simple imputations
#'
#' Wrapper for \code{\link[base]{sample}}
#'
#' @export
#' @param n The number of missing values to impute.
#' @param vals The values to sample from (with replacement).
#' @param weight_var Variable from which to compute relative frequencies of \code{vals}
#' to use as \code{prob} argument to \code{\link[base]{sample}}. If missing then
#' the values in \code{vals} are sampled with equal probability.
#'
#' @return A vector of length \code{n}.
#'
#'
simple_impute <- function(n, vals, weight_var) {
  weights <- if (!missing(weight_var))
    as.numeric(table(weight_var)) else NULL
  sample(x = vals, size = n, replace = TRUE, prob = weights)
}
