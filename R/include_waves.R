#' Add to vector of baseline variable names the names of corresponding variables
#' in later survey waves
#'
#' @export
#' @param vars A character vector of (baseline) variable names
#' @param waves A numeric vector of survey wave numbers
#' @return \code{vars} updated to include the the names of the corresponding
#'   variables in the waves indicated by \code{waves}.
#'
include_waves <- function(vars, waves = NULL) {
  if (!is.numeric(waves))
    stop("wave should a be numeric vector")

  all_vars <- list(vars)
  for (j in seq_along(waves))
    all_vars[[j + 1]] <- paste0("q", waves[j], vars)

  unlist(all_vars)
}
