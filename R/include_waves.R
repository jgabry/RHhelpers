#' Take baseline variable names and return variable names for baseline and/or
#' later waves
#'
#' @export
#' @param vars A character vector of (baseline) variable names
#' @param waves A numeric vector of survey wave numbers (0 for baseline)
#' @param year A numeric integer of the year of survey collection
#' @return \code{vars} updated to include the the names of the corresponding
#'   variables in the waves indicated by \code{waves}.
#'
include_waves <- function(vars, waves = 0, year = NA) {
  if (!is.numeric(waves))
    stop("wave should a be numeric vector")
  if (length(year)>1)
    stop("can only impute 1 year at a time")

  waves <- sort(waves)
  all_vars <- list(vars)
  if (any(waves != 0)) {
    for (j in seq_along(waves))
      all_vars[[j + 1]] <- paste0("q", waves[j], vars)
  }
  all_vars <- unlist(all_vars)
  if (!0 %in% waves)
    all_vars <- all_vars[!all_vars %in% vars]

  if(!is.na(year)){
    if(any(waves == 0)) stop("Waves start at 1 in harmonized naming convention")
    all_vars <- paste0("y",year,"_",all_vars)
    all_vars
  }else{
    all_vars[!grepl("^q0", all_vars)]
  }
}
