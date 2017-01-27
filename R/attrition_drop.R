#' Replace imputations with NAs if respondent did not participate in a survey wave
#'
#' @export
#' @param imp_df Data frame from after imputation
#' @param wave The wave number
#' @param wave_indicator A logical vector. Each element should be
#'   TRUE if the respondent is in the wave and FALSE if not.
#' @return Updated \code{imp_df}.
#'
attrition_drop <- function(imp_df, wave, wave_indicator) {
  stopifnot(is.numeric(wave), is.logical(wave_indicator))
  sel <- grep(paste0("^q", wave), colnames(imp_df))
  imp_df[!wave_indicator, sel] <- NA
  imp_df
}
