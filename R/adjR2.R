#' Adjusted R-squared
#'
#' adjR2(pred, truth, d) takes inputs of logical or binary vectors of predicted and true classes as well as
#' the number of predictors, and returns the adjusted R-squared value
#' @param pred a binary or logical vector of predicted classes
#' @param truth a binary or logical vector of observed classes
#' @param d the number of predictors used in prediction
#' @return adjR2
#' @examples
#' pred = c(1, 1, 1, 0, 0, 0)
#' truth = c(1, 0, 1, 0, 0, 0)
#' adjR2(pred, truth, 1)
#' @export
#' @rdname adjR2

adjR2 = function(pred, truth, d){
  n = length(truth)
  RSS = sum((truth - pred)^2)/(n - d - 1)
  TSS = sum((truth - mean(truth))^2)/(n - 1)
  adjR2 = 1 - (RSS/TSS)
  return(adjR2)
}
