#' Mean plus \eqn{k} standard deviations
#'
#' @export
#' @param x numeric vector.
#' @param k number of standard deviations.
#' @return \code{mean(x) + k * sd(x)}
#'
mean_plus_sds <- function(x, k) {
  stopifnot(is.numeric(x))
  mean(x) + k * sd(x)
}
