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