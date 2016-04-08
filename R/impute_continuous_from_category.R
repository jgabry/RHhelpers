#' Naively impute continuous value given a bracket (i.e., bounds)
#'
#' @export
#' @description Naive imputation of earnings (and income from other family
#'   members in HH) for respondents who provided a bracket but not the actual
#'   value. This draws a value from a uniform distribution with the bounds
#'   implied by the bracket.
#'
#' @param df The data frame to use.
#' @param contvars Character vector of names of the continuous variables with
#'   missing values.
#' @param catvars Character vector of names of categorical variables indicating
#'   the bracket.
#' @param breaks The breakpoints for all the brackets.
#' @return An updated \code{df} including the imputations.
#'
naive_imputation_step <- function(df, contvars, catvars, breaks) {
  stopifnot(length(contvars) == length(catvars), is.data.frame(df))
  for (j in seq_along(contvars)) {
    df <- impute_continuous_from_category(contvar = contvars[j],
                                          catvar = catvars[j],
                                          df = df,
                                          breaks = breaks)
  }
  df
}

# internal function to do imps for a single contvar/catvar pair
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


