#' Create imputation information list
#'
#' @export
#' @param vars Character vector of variable names
#' @param types Character vector of variable types
#' @param bounds List of bounds, or NAs. E.g. list(NA, NA, NA, c(0, Inf))
#' @param include_q4 Should the q4 versions of vars be included?
#' @return A list containing info on the type of variable and any bounds
#'
imp_varlist <- function(vars, types, bounds = list(), include_q4 = FALSE) {
  stopifnot(length(vars) == length(types) && length(vars) == length(bounds))
  L <- length(vars)
  if (include_q4) {
    L <- 2 * L
    vars <- c(vars, paste0("q4", vars))
    types <- rep(types, 2)
    bounds <- rep(bounds, 2)
  }
  varlist <- setNames(vector(mode = "list", length = L), nm = vars)
  for (j in seq_along(varlist)) {
    varlist[[j]] <- list(type = types[j], bounds = bounds[[j]])
  }
  return(varlist)
}
