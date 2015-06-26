#' Trim high survey weights using quantile as upper bound
#'
#' Wrapper for \code{\link[survey]{trimWeights}}
#'
#' @export
#' @param design A survey design object.
#' @param prob A probability in \eqn{[0,1]}
#' @param ... Additional arguments to pass to \code{\link[survey]{trimWeights}}
#' @return See \code{\link[survey]{trimWeights}}
upper_quantile_trimming <- function(design, prob, ...) {
  wts <- weights(design)
  trimWeights(design, lower = -Inf, upper = quantile(wts, probs = prob), ...)
}

