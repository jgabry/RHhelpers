#' Print status messages
#' 
#' @export 
#' @param msg character string. The message to print. 
status <- function(msg) {
  cat(rep("-", 25), "\n", msg, 
      "\n", rep("-", 25), 
      "\n", sep = "")
}