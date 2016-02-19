#' Convert NAs in imputation flags to zeros
#'
#' This function takes a data.frame \code{df} and converts all \code{NA}s in
#' the imputation flag variables to zeros.
#'
#' @export
#' @param df A data frame
#' @param patt Pattern to \code{grep} for. Used to find the column names
#'   corresponding to the imputation flag variables.
#' @return \code{df}, updated
#'
NAs_to_zeros <- function(df, patt = "pimp.") {
  sel <- grep(patt, colnames(df), fixed = TRUE)
  if (!length(sel))
    stop("No variables with ", patt, " in the names found")

  for (j in seq_along(sel)) {
    d <- df[, sel[j]]
    d[is.na(d)] <- 0
    df[, sel[j]] <- d
  }
  df
}
