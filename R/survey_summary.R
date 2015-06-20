#' Print survey summary statistics
#' 
#' @export 
#' @param x variable to use. 
#' @param which character string. The function paste0("svy", which) will be used.
#' @param design survey design object. 
#' @param msg character string. An optional message to print. 
#' 
survey_summary <- function(x, which = "mean", design, msg = NULL) {
  if (!is.null(msg)) cat(msg, "\n")
  args <- list(x = x, design = design)
  print(out <- do.call(paste0("svy", which), args))
  return(invisible(out))
}