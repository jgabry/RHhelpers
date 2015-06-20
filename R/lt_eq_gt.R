#' Recode variable based on whether it is less than, equal to, or 
#' greater than a specified comparison value 
#' 
#' @export 
#' @param x a numeric vector. The variable to recode.
#' @param k a single numeric value. The comparison value. 
#' @param vals a vector of length 3. Defaults to \code{c(1,2,3)}.
#' @return Returns \code{vals[1]} if \code{x < k}, \code{vals[2]} if 
#' \code{x == k}, and \code{vals[3]} if \code{x > k}. The function is vectorized
#' so that if \code{length(x) > 1} each \code{x[i]} is compared to \code{k} and
#' recoded. 
#' 
lt_eq_gt <- function(x, k, vals = 1:3) {
  stopifnot(length(vals) == 3)
  stopifnot(is.numeric(k))
  ifelse(x < k, vals[1], ifelse(x == k, vals[2], vals[3]))
}