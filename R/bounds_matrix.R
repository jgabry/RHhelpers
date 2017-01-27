#' Make bounds matrix for amelia
#'
#' @export
#' @param varlist A list returned by \code{\link{imp_varlist}}.
#' @return A matrix that can be passed to the \code{bounds} argument of the
#'   \code{\link[Amelia]{amelia}} function.
#'
bounds_matrix <- function(varlist) {
  bounds <- lapply(varlist, "[[", "bounds")
  mark <- which(!is.na(sapply(bounds, "[[", 1)))
  b <- matrix(NA, nrow = length(mark), ncol = 3)
  b[, 1] <- mark # column number of variable
  b[, 2] <- sapply(bounds[mark], "[[", 1) # lower bound
  b[, 3] <- sapply(bounds[mark], "[[", 2) # upper bound
  colnames(b) <- c("col", "lb", "ub")
  rownames(b) <- names(mark)
  return(b)
}
