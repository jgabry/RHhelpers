.conditions <- function(...) {
  as.list(substitute(list(...))[-1])
}
.na_to_false <- function(x) {
  if (is.list(x)) {
    x <- lapply(seq_along(x), function(i) {
      xi <- x[[i]]
      xi[is.na(xi)] <- FALSE
      xi
    })
  } else {
    x[is.na(x)] <- FALSE
  }
  x
}

Ix <- function(conditions, and = TRUE, df = NULL,
               na_is_false = FALSE,
               type = c("logical", "numeric", "integer")) {

  if (!is.list(conditions))
    conditions <- as.list(conditions)
  if (!is.null(df))
    stopifnot(is.data.frame(df))
  operator <- if (and) '&' else '|'
  as.type <- paste0("as.", match.arg(type))
  res <- lapply(conditions, eval, envir = df, enclos = parent.frame())
  L <- length(res)

  if (na_is_false)
    res <- .na_to_false(res)
  sums <- rowSums(do.call(cbind, res))
  out <- if (and) sums == L else sums >= 1
  if (as.type != "as.logical")
    do.call(as.type, list(out))
  else
    out
}
