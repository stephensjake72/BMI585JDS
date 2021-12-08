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