#' Make a one-column matrix of 1s
#'
#' @export
#' @param sample_size The number of rows for the matrix.
#' @return \code{matrix(1, sample_size, 1)}
#'
ones <- function(sample_size) {
  matrix(1, nrow = sample_size, ncol = 1)
}

