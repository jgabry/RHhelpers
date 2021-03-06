#' Convert (one or more) 1/2 indicator variables to 1/0 indicator variables
#'
#' NOTE: This converts the 2s to 0s and leaves the 1s and 1s!!
#'
#' @export
#' @param vars A character vector of variable names
#' @param df The data frame in which to look for \code{vars}
#' @return \code{df}, with \code{vars} converted to 0/1 indicators (binary
#'   variables)
#'
indicator12_to_indicator01 <- function(vars, df) {
  stopifnot(is.data.frame(df), is.character(vars))
  for (v in vars) {
    df[[v]] <- ifelse(df[[v]] == min(df[[v]], na.rm = TRUE), 1, 0)
  }
  df
}
