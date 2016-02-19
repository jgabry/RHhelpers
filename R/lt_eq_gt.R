#' Recode variable based on whether it is less than, equal to, or
#' greater than a specified comparison value
#'
#' @export
#' @param x a numeric vector. The variable to recode.
#' @param compare_to a single numeric value. The comparison value.
#' @param vals a vector of length 3. Defaults to \code{c(1,2,3)}.
#' @return Returns \code{vals[1]} if \code{x < compare_to}, \code{vals[2]} if
#'   \code{x == compare_to}, and \code{vals[3]} if \code{x > compare_to}. The
#'   function is vectorized so that if \code{length(x) > 1} each \code{x[i]} is
#'   compared to \code{k} and recoded.
#'
#' @examples
#' y1 <- 1:10
#' y2 <- lt_eq_gt(y1, compare_to = 5)
#'
lt_eq_gt <- function(x, compare_to, vals = 1:3) {
  k <- compare_to
  stopifnot(length(vals) == 3, is.numeric(k))
  ifelse(x < k, vals[1], ifelse(x == k, vals[2], vals[3]))
}
