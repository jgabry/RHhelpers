#' Make a one-column matrix of 1s
#'
#' @export
#' @param sample_size The number of rows for the matrix.
#' @return A one column matrix of 1s with \code{sample_size} rows.
#'
ones <- function(sample_size) {
  matrix(1, nrow = sample_size, ncol = 1)
}

