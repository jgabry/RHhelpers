#' Get names of imputation flag variables
#'
#' @export
#' @param vars Character vector of variable names
#' @param flag The text to use when creating the names of flag variables. If
#' not baseline, then "qx" will be prepended to \code{flag}, where "x" is
#' the appropriate wave.
#' @param waves A numeric vector indicating any survey waves beyond baseline for
#'   which variable names should be looked for in \code{vars} and corresponding
#'   flag names returned
#'
#' @return Character vector of names for the imputation flag variables
#'   corresponding to vars
#'
#' @examples
#' vars <- c("bob", "q4bob", "q8bob", "jim", "q4jim")
#' flag_names(vars)
#' flag_names(vars, waves = 4) # q8 vars treated like baseline because 8 not in waves
#'
flag_names <- function(vars, flag = "pimp.", waves = 1:12) {
  if (!is.numeric(waves)) {
    return(paste0(flag, vars))
  }

  wave_vars <- paste0("q", waves)
  flags <- list()
  for (j in seq_along(vars)) {
    w <- which(wave_vars == paste0('q',gsub("[^0-9]", "",  substr(vars[j],1,3))))
    if (!length(w)) {
      flags[[j]] <- paste0(flag, vars[j])
    } else {
      patt <- wave_vars[w]
      repl <- paste0(wave_vars[w], flag)
      flags[[j]] <- sub(patt, repl, vars[j])
    }
  }
  unlist(flags)
}
