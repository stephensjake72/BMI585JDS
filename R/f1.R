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