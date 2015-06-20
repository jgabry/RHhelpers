#' Placeholder NA-filled data.frame 
#' 
#' @export 
#' @param nrow,ncol the number of rows and columns for the \code{data.frame}
#' @return an \code{nrow} by \code{ncol} \code{data.frame} of NAs. 
#' 
empty_df <- function(nrow, ncol) {
  as.data.frame(matrix(NA, nrow, ncol))  
}