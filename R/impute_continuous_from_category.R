#' Impute continuous value given a bracket (i.e., bounds)
#'
#' Used for naive imputation of earnings (and income from other family members
#' in HH) for respondents who provided a bracket but not the actual value. This
#' draws a value from a uniform distribution with the bounds implied by the
#' bracket.
#'
#' @export
#' @param contvar Name of the continuous variable with missing values
#' @param catvar Name of the categorical variable indicating the bracket
#' @param df The data frame in which to look for \code{contvar} and \code{catvar}
#' @param breaks The breakpoints for all the brackets
#' @return An updated version of \code{df} including the imputations
#'
impute_continuous_from_category <- function(contvar, catvar, df, breaks) {
  cat_num <- as.numeric(df[[catvar]])
  ncats <- length(unique(na.omit(cat_num)))
  stopifnot(length(breaks) == ncats + 1)
  for (j in 1:ncats) {
    catj <- !is.na(cat_num) & cat_num == j
    n <- sum(catj)
    impj <- runif(n, breaks[j], breaks[j + 1])
    df[catj, contvar] <- impj
  }
  return(df)
}

