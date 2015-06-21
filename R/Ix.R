.conditions <- function(...) {
  as.list(substitute(list(...))[-1])
}
.NA_to_FALSE <- function(x) {
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
               NA_is_FALSE = FALSE,
               type = c("logical", "numeric", "integer")) {

  if (!is.list(conditions))
    conditions <- as.list(conditions)
  if (!is.null(df))
    stopifnot(is.data.frame(df))
  operator <- if (and) '&' else '|'
  as.type <- paste0("as.", match.arg(type))
  res <- lapply(conditions, eval, envir = df, enclos = parent.frame())
  L <- length(res)

  if (NA_is_FALSE)
    res <- .NA_to_FALSE(res)

  out <- rowSums(do.call(cbind, res)) == L
  if (as.type != "as.logical")
    do.call(as.type, list(out))
  else
    out
}
