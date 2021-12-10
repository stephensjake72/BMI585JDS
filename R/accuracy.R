#' Classification accuracy
#'
#' accuracy(pred, truth) takes inputs of logical or binary vectors of predicted and true classes
#' and returns the accuracy of the predictions, calculated as
#' the sum of correct positive classifications and correct negative classifications
#' divided by the total number of observations
#' @param pred a binary or logical vector of predicted classes
#' @param truth a binary or logical vector of observed classes
#' @return accuracy
#' @examples
#' accuracy(c(1, 1, 1, 0, 0, 0), c(1, 0, 1, 0, 0, 0))
#' accuracy(c(T, T, T, F, F, F), c(T, F, T, F, F, F))
#' @export
#' @rdname accuracy

accuracy = function(pred, truth){
  # load predictions and truth as logicals
  pred = as.logical(pred)
  truth = as.logical(truth)

  # number of true positive classifications
  ntp = length(which(pred == T & truth == T))
  # number of true negative classifications
  ntn = length(which(pred == F & truth == F))

  # number of total predictions
  n = length(pred)

  # return accuracy
  return((ntp + ntn)/n)
}
