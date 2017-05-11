#' Save weights as .rds or .dta file
#'
#' This will save all variables with names containing 'weight_'
#' @export
#' @param data the \code{data.frame} to take the weights from.
#' @param format save as .rds file (R) or .dta file (Stata).
#' @param file_name the path to where the file should be saved.
#' @param include_reps logical. Should replicate weights by saved if found?
#' @return a .dta or .rds file, depending on \code{format}.
save_weights <- function(data, format = c("rds","dta"),
                         file_name = paste0("weights.",format),
                         include_reps = TRUE) {

  ext <- match.arg(format)
  L <- nchar(file_name)
  file_end <- substr(file_name, L - nchar(ext) + 1, L)
  if (file_end != ext) {
    file_name <- paste0(file_name, ".", ext)
  }

  cnms <- colnames(data)
  weight_cols <- cnms[grep("weight_", cnms)]
  if (!include_reps) {
    rep_cols <- grep("rep", weight_cols)
    if (!identical(rep_cols, integer(0)))
      weight_cols <- weight_cols[-rep_cols]
  }
  weights <- subset(data, sel = c("subject_id", weight_cols))
  weights <- dplyr::arrange(weights, subject_id)
  cat(paste0("Writing file '", file_name, "'"))

  if (format == "rds") {
    attr(weights, "date") <- Sys.Date()
    saveRDS(weights, file = file_name)
  } else {
    foreign::write.dta(weights, file = file_name)
  }
}
