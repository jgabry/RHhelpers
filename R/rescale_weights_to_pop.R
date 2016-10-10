#' Rescale weights to sum to population
#'
#' @export
#' @param x vector of weights
#' @param pop_size population size
#' @return a vector that sums to \code{pop_size}
#'
rescale_weights_to_pop <- function(x, pop_size) {
  sum_to_one <- x / sum(x, na.rm = TRUE)
  sum_to_one * pop_size
}
