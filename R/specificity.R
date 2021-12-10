#' Specificity
#'
#' specificity(pred, truth) returns the specificity of the predictor according to predicted class 'pred' and observed classes 'truth'.
#' 'pred' and 'truth' can be either binary or logical vectors.
#' Specificity is computed as the number of correct negative predictions divided by the total number of negative predictions.
#'
#' @param pred a binary or logical vector of predicted classes
#' @param truth a binary or logical vector of observed classes
#' @return specificity
#' @examples
#' pred = c(1, 1, 1, 0, 0, 0)
#' truth = c(1, 0, 1, 0, 0, 0)
#' specificity(pred, truth)
#'
#' pred = c(T, T, T, F, F, F)
#' truth = c(T, F, T, F, F, F)
#' specificity(pred, truth)
#' @export
#' @rdname specificity

specificity = function(pred, truth){
  # load predictions and truth as logicals
  pred = as.logical(pred)
  truth = as.logical(truth)

  # number of true negative classifications
  ntn = length(which(pred == F & truth == F))
  # total true negative cases
  nn = length(which(truth == F))

  # return specificity
  return(ntn/nn)
}
