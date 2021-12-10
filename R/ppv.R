#' Positive Predictive Value
#'
#' ppv(pred, truth) return the positive predictive value of predicted classes 'pred' and observed classes 'truth'.
#' PPV is computed as the number of correct positive classifications divided by the sum of correct positive and false positive predictions.
#'
#' @param pred a binary or logical vector of predicted classes
#' @param truth a binary or logical vector of observed classes
#' @return positive predictive value
#' @examples
#' pred = c(1, 1, 1, 0, 0, 0)
#' truth = c(1, 0, 1, 0, 0, 0)
#' ppv(pred, truth)
#'
#' pred = c(T, T, T, F, F, F)
#' truth = c(T, F, T, F, F, F)
#' ppv(pred, truth)
#'
#' @export
#' @rdname ppv

ppv = function(pred, truth){
  # load predictions and truth as logicals
  pred = as.logical(pred)
  truth = as.logical(truth)

  # number of true positive classifications
  ntp = length(which(pred == T & truth == T))
  # number of false positive classifications
  nfp = length(which(pred == T & truth == F))

  # return positive predictive value
  return(ntp/(ntp + nfp))
}
