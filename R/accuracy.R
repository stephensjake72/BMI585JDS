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