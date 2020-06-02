#' Make all values of vars with imputation flag = 1 into NAs
#'
#' @export
#' @param df The data.frame to use
#' @param vars Character vector of variable names
#' @param flag,waves See \code{\link{flag_names}}
#' @return A modified version of df with all flagged values of vars made NA
#'
prepare_imp_NAs <- function(df, vars, flag = "pimp.", waves = 1:12) {
  # get names of imputation flag variables
  # split vars into cross-sectional and longitudinal formats:
  c_vars <- vars[grep('^y(\\d{2})',vars)] #cross-section var names
  if(length(c_vars)){
    l_vars <- vars[grep('^y(\\d{2})',vars,invert=TRUE)] #longitudinal var names
    c_flags <- flag_names(c_vars, flag = flag, waves = waves, year = 15:20)
    if(length(l_vars)){
      l_flags <- flag_names(l_vars, flag = flag, waves = waves, year = NA)
      flags <- c(l_flags,c_flags)
      vars <- c(l_vars,c_vars) #need to be matched in location to flags
    }
    flags <- c_flags
    vars <- c_vars
  }else{
    flags <- flag_names(vars, flag = flag, waves = waves, year = NA)
  }


  # check for variables not found
  all_nms <- c(vars, flags)
  bad <- which(!all_nms %in% colnames(df))
  if (length(bad)) {
    stop("Can't find some variable in 'df': ",
         paste(all_nms[bad], collapse = ", "))
  }

  # set values to NA if imputation flag = 1
  for (j in seq_along(vars)) {
    v <- vars[j]
    f <- flags[j]
    mark <- which(df[[f]] == 1)
    df[mark, v] <- NA
  }
  return(df)
}
