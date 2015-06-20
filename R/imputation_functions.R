#' Functions used for multiple imputation
#' @name imputation
#' @export
#' @param df a \code{data.frame}.
#' @param cl cluster.
#' @param seeds seeds.
#' @param impmthd a vector of strings with length ncol(data), specifying the
#'   elementary imputation method to be used for each column in data imputation
#'   methods. See \code{\link[mice]{mice}}.
#' @param prd_mat a square binary matrix. See \code{\link[mice]{mice}}.
#'
par_mice <- function(df, cl, seeds, impmthd, prd_mat) {
  #parallelized multiple imputation
  imp_pars <- parallel::parLapply(cl = cl, X = seeds, fun = function(seed){ 
    mice::mice(df, m = 3, maxit=40, imputationMethod = impmthd, predictorMatrix = prd_mat,
         visitSequence= "monotone", diagnostics = T, printFlag = TRUE, seed = seed)})
  #values from first node, then add subsequent to list
  imp_merged <- imp_pars[[1]] 
  for (n in 2:length(imp_pars)) {
    imp_merged <- mice::ibind(imp_merged, imp_pars[[n]])
  }
  merged_df <- df #original data
  merged_df <-cbind(data.frame(.imp = 0, .id = 1:nrow(df)), merged_df) #add empty columns of imputation variables
  #concatinate
  for (n in 1:length(imp_pars)){
    tmp <- mice::complete(imp_pars[[n]], action = "long")
    tmp$.imp <- as.numeric(tmp$.imp) + max(merged_df$.imp)
    merged_df <- rbind(merged_df,tmp)}
  merged_df.imps <- merged_df[merged_df$.imp!=0,] #remove original values with 'NA'
  return(merged_df.imps)
}


#' @rdname imputation
#' @export
#' @param all_imps data for all imputations
#'
imp_reduce <- function(all_imps){
  #get single output values from multiple estimates
  factor_vars <- colnames(all_imps)[lapply(all_imps,class)=='factor'] 
  #median values for numeric
  tmp <- aggregate(all_imps[!colnames(all_imps) %in% factor_vars], by=list(all_imps$.id),median)
  #mode values for factors
  tmp2 <- aggregate(all_imps[,factor_vars], by=list(all_imps$.id), Mode) #
  #merge and reorder
  data_imp_complete_unordered <- merge(tmp, tmp2, by = "Group.1", all = TRUE)
  data_imp_complete <- data_imp_complete_unordered[ order(data_imp_complete_unordered[,'subject.id']),]
  return(data_imp_complete)
}


#' @rdname imputation
#' @export
gateway_zeros <- function(x,y,df){
  x <- as.character(x) #ensure we have variable names as strings
  y <- as.character(y)
  logic <- df[,y] == 0 #identify which observations estimated as w/o a given asset
  logic[is.na(logic)] <- FALSE #extend to missing values
  df[,x][logic] <- 0
  var.gate.transformed <- df[,x]
  return(var.gate.transformed)
}


#' @rdname imputation
#' @export
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#' @rdname imputation
#' @export
nm <- function(x) deparse(substitute(x)) 