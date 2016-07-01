#' Create imputation information list
#'
#' @export
#' @param vars Character vector of variable names
#' @param types Character vector of variable types
#' @param bounds List of bounds, or NAs. E.g. list(NA, NA, NA, c(0, Inf))
#' @param waves A numeric vector of survey wave numbers indicating waves to
#'   include in addition to baseline
#' @return A list containing info on the type of variable and any bounds
#'
imp_varlist <- function(vars, types, bounds = list(), waves = 0) {
  stopifnot(length(vars) == length(types) && length(vars) == length(bounds))
  L <- length(vars)
  if (!all(waves == 0)) {
    W <- length(waves)
    L <- L * W
    vars <- include_waves(vars, waves)
    types <- rep(types, W)
    bounds <- rep(bounds, W)
  }
  varlist <- setNames(vector(mode = "list", length = L), nm = vars)
  for (j in seq_along(varlist))
    varlist[[j]] <- list(type = types[j], bounds = bounds[[j]])

  return(varlist)
}
