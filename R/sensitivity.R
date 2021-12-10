#' Sensitivity
#'
#' sensitivity(pred, truth) returns the sensitivity of the predictor according to predicted class 'pred' and observed classes 'truth'.
#' 'pred' and 'truth' can be either binary or logical vectors.
#' Sensitivity is computed as the number of correct positive predictions divided by the total number of positive predictions.
#'
#' @param pred a binary or logical vector of predicted classes
#' @param truth a binary or logical vector of observed classes
#' @return sensitivity
#' @examples
#' pred = c(1, 1, 1, 0, 0, 0)
#' truth = c(1, 0, 1, 0, 0, 0)
#' sensitivity(pred, truth)
#'
#' pred = c(T, T, T, F, F, F)
#' truth = c(T, F, T, F, F, F)
#' sensitivity(pred, truth)
#' @export
#' @rdname sensitivity

sensitivity = function(pred, truth){
  # load predictions and truth as logicals
  pred = as.logical(pred)
  truth = as.logical(truth)

  # get number of correct positive classifications and total true positives
  ntp = length(which(pred == T & truth == T))
  np = length(which(truth == T))

  # return sensitivity
  return(ntp/np)
}
