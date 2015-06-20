#' Histogram of weights
#' 
#' @export 
#' @param w vector of weights. 
#' @param title character string. The title for the plot.
#'
weights_hist <- function(w, title = NULL) {
  hist(w, xlab = "weights", ylab = NULL, main = title, col = "skyblue", yaxt = "n")
}