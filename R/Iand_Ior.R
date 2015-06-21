#' Indicator variables
#'
#' For \code{Iand} and \code{Ior} the expressions in \code{...}
#' are linked by \code{&} and \code{|}, respectively.
#'
#' @export
#' @param ... logical expressions.
#' @param df a \code{data.frame}.
#' @param NA_is_FALSE logical. If \code{TRUE}, expressions in \code{...} that
#' evaluate to \code{NA} will be treated as \code{FALSE}, and thus the returned
#' vector will not contain any \code{NA} values. If \code{TRUE} then an \code{NA}
#' will be returned whenever any of the expressions evaluates to \code{NA}. See
#' Details.
#' @param type The desired type for the result. One of \code{'logical'}
#'   (default), \code{'numeric'}, or \code{'integer'}.
#'
#' @return A logical (default), numeric, or integer indicator variable, depending on
#' the value of \code{type}.
#'
#' @details Suppose \code{x} and \code{y} are numeric vectors and \code{z} is
#' a character vector. If any of the vectors contain \code{NA} values then
#'
#' \code{I_1 <- Iand(x < y, z == "A", df = data, NA_is_FALSE = TRUE)}
#'
#' \code{I_2 <- Iand(x < y, z == "A", df = data, NA_is_FALSE = FALSE)}
#'
#' are not the same. For example:
#'
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
#' ex1 <- Iand(x < y, z == "A", df = data, NA_is_FALSE = TRUE)
#' ex2 <- Iand(x < y, z == "A", df = data, NA_is_FALSE = FALSE)
#' cbind(data, ex1, ex2)
#'
Iand <- function(..., df = NULL, NA_is_FALSE = FALSE,
                 type = c("logical", "numeric", "integer")) {
  Ix(.conditions(...), df = df, NA_is_FALSE = NA_is_FALSE,
     type = type, and = TRUE)
}

#' @rdname Iand
#' @export
Ior <- function(..., df = NULL, NA_is_FALSE = FALSE,
                type = c("logical", "numeric", "integer")) {
  Ix(.conditions(...), df = df, NA_is_FALSE = NA_is_FALSE,
     type = type, and = FALSE)
}
