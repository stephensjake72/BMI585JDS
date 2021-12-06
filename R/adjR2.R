adjR2 = function(pred, truth, d){
  n = length(truth)
  RSS = sum((truth - pred)^2)/(n - d - 1)
  TSS = sum((truth - mean(truth))^2)/(n - 1)
  adjR2 = 1 - (RSS/TSS)
  return(adjR2)
}