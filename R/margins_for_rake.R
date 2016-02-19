#' Sample and population margins for raking
#'
#' This function can be used to construct the \code{sample.margins} and
#' \code{population.margins} arguments for \code{\link[survey]{rake}}
#'
#' @export
#' @param me_vars character vector of variable names for main effects.
#' @param int_vars character vector naming variables to be interacted with
#' \code{interact_with}.
#' @param interact_with character string naming the variable with which
#' the variables in \code{int_vars} will be interacted. Currently
#' \code{interact_with} can only be a single variable.
#' @param design survey object. See \code{\link[survey]{svydesign}}.
#'
#' @return A list with components \code{samp} and \code{pop}. \code{samp} is a
#'   list of formulas describing sample margins. \code{pop} is a list of
#'   data.frames and tables describing the corresponding population margins. See
#'   the description of the \code{sample.margins} and \code{population.margins}
#'   arguments for \code{\link[survey]{rake}}.
#'
margins_for_rake <- function(me_vars, int_vars, interact_with, design) {
  samp.marg <- list()
  pop.marg <- list()
  for (i in seq_along(me_vars)) {
    f <- as.formula(paste("~", me_vars[i]))
    samp.marg[[i]] <- f
    pop.marg[[i]] <- as.data.frame(svytable(f, design = design))
  }
  for (i in seq_along(int_vars)) {
    k <- i + length(me_vars)
    f <- as.formula(paste("~", int_vars[i], paste("+", interact_with)))
    samp.marg[[k]] <- f
    pop.marg[[k]] <- svytable(f, design = design)
  }
  list(samp = samp.marg, pop = pop.marg)
}
