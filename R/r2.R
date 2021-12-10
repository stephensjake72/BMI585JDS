#' R-squared
#'
#' r2(pred, truth, d) takes inputs of logical or binary vectors of predicted and true classes
#' and returns the R-squared value according to residual and total sum of squares error.
#'
#' @param pred a binary or logical vector of predicted classes
#' @param truth a binary or logical vector of observed classes
#' @return R2
#' @examples
#' pred = c(1, 1, 1, 0, 0, 0)
#' truth = c(1, 0, 1, 0, 0, 0)
#' r2(pred, truth, 1)
#' @export
#' @rdname r2

r2 = function(pred, truth){
  RSS = sum((truth - pred)^2)
  TSS = sum((truth - mean(truth))^2)
  R2 = 1 - (RSS/TSS)
  return(R2)
}
