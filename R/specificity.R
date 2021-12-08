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