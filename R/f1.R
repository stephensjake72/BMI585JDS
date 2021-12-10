#' F1 score
#'
#' f1(pred, truth) returns the F1 score of the predicted class variable 'pred' and observed classes 'truth'.
#' 'pred' and 'truth' can be either binary or logical vectors. Relies upon ppv() and sensitivity() functions.
#'
#' @param pred a binary or logical vector of predicted classes
#' @param truth a binary or logical vector of observed classes
#' @return f1 score
#' @examples
#' pred = c(1, 1, 1, 0, 0, 0)
#' truth = c(1, 0, 1, 0, 0, 0)
#' f1(pred, truth)
#'
#' pred = c(T, T, T, F, F, F)
#' truth = c(T, F, T, F, F, F)
#' f1(pred, truth)
#' @export
#' @rdname f1

f1 = function(pred, truth){
  # load predictions and truth as logicals
  pred = as.logical(pred)
  truth = as.logical(truth)

  # get positive predictive value and sensitivity
  precision = ppv(pred, truth)
  recall = sensitivity(pred, truth)

  # return f1 score
  return(2*precision*recall/(precision + recall))
}
