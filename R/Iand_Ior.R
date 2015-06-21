#' Indicator variables
#'
#' For \code{Iand} and \code{Ior} the expressions in \code{...} are linked by
#' \code{&} and \code{|}, respectively.
#'
#' @export
#' @param ... logical expressions.
#' @param df a \code{data.frame}.
#' @param na_is_false logical. Treat \code{NA} values as false? If \code{TRUE},
#'   expressions in \code{...} that evaluate to \code{NA} will be treated as
#'   false, and thus the returned vector will not contain any \code{NA} values.
#'   If \code{FALSE} then an \code{NA} will be returned whenever any of the
#'   expressions evaluates to \code{NA}. See Details and Examples.
#' @param type The desired type for the result. One of \code{'logical'}
#'   (default), \code{'numeric'}, or \code{'integer'}.
#'
#' @return A logical (default), numeric, or integer indicator variable,
#'   depending on the value of \code{type}.
#'
#' @details Suppose \code{x} and \code{y} are numeric vectors and \code{z} is
#' a character vector. If any of the vectors contain \code{NA} values then
#'
#' \code{I_1 <- Iand(x < y, z == "A", df = data, na_is_false = TRUE)}
#'
#' \code{I_2 <- Iand(x < y, z == "A", df = data, na_is_false = FALSE)}
#'
#' are not the same. For example:
#' \tabular{ccccccc}{
#'   \code{x < y}  \tab \code{z}  \tab\tab \code{I_1} \tab \code{I_2} \cr
#'   T  \tab A  \tab\tab  T \tab  T\cr
#'   T  \tab B  \tab\tab F \tab F\cr
#'   T  \tab NA \tab\tab F \tab    NA\cr
#'   F  \tab A  \tab\tab F \tab F\cr
#'   F \tab B  \tab\tab F \tab F\cr
#'   F  \tab NA \tab\tab F \tab    NA
#' }
#'
#' @examples
#' data <- data.frame(x = 0, y = rep(c(1,-1), each = 3), z = rep(c('A','B',NA), 2))
#' ex1 <- Iand(x < y, z == "A", df = data, na_is_false = TRUE)
#' ex2 <- Iand(x < y, z == "A", df = data, na_is_false = FALSE)
#' cbind(data, ex1, ex2)
#'
Iand <- function(..., df = NULL, na_is_false = FALSE,
                 type = c("logical", "numeric", "integer")) {
  Ix(.conditions(...), df = df, na_is_false = na_is_false,
     type = type, and = TRUE)
}

#' @rdname Iand
#' @export
Ior <- function(..., df = NULL, na_is_false = FALSE,
                type = c("logical", "numeric", "integer")) {
  Ix(.conditions(...), df = df, na_is_false = na_is_false,
     type = type, and = FALSE)
}
