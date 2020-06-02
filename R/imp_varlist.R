#' Create list of information about variables to be imputed
#'
#' @export
#' @param vars Character vector of variable names.
#' @param types Character vector of variable types. Can include \code{"num"}
#'   (numeric), \code{"int"} (interger), \code{"ordinal"}, \code{"nominal"},
#'   \code{"binary"}.
#' @param bounds List of bounds, or NAs. For example, \code{list(NA, NA, NA,
#'   c(0, Inf))} if there are four variables and the first three are unbounded
#'   and the fourth is non-negative.
#' @param waves A numeric vector of survey wave numbers indicating waves to
#'   include in addition to baseline. The default \code{0} just includes
#'   baseline.
#' @return A list containing info on the type of variable and any bounds. This
#'   information is later used when setting up the arguments to pass to the
#'   the Amelia package for imputation.
#'
imp_varlist <- function(vars, types, bounds = list(), waves = 0, year = NA) {
  stopifnot(length(vars) == length(types) && length(vars) == length(bounds))
  if(!all(length(NA)==1)) stop("Can only impute one year at a time")
  L <- length(vars)
  if(is.na(year)){
    print("Assuming longitudinal naming convention")
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
  } else{
      print("Assuming harmonized naming convention")
      if(any(waves==0)) stop("Naming covention for harmonized convention names first wave as 1")
      if (!all(waves == 0)) {
        W <- length(waves)
        L <- L * W
        vars <- include_waves(vars, waves, year)
        types <- rep(types, W)
        bounds <- rep(bounds, W)
      }
      varlist <- setNames(vector(mode = "list", length = L), nm = vars)
      for (j in seq_along(varlist))
        varlist[[j]] <- list(type = types[j], bounds = bounds[[j]])
  }
  return(varlist)
}
