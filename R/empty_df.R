#' Create a placeholder NA-filled data.frame
#'
#' @export
#' @param nrow,ncol the number of rows and columns for the \code{data.frame}
#' @return an \code{nrow} by \code{ncol} \code{data.frame} of NAs.
#'
#' @examples
#' d <- empty_df(3, 2)
#' print(d)
#'
empty_df <- function(nrow, ncol) {
  as.data.frame(matrix(NA, nrow, ncol))
}
