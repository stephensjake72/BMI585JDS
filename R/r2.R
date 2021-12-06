r2 = function(pred, truth){
  RSS = sum((truth - pred)^2)
  TSS = sum((truth - mean(truth))^2)
  R2 = 1 - (RSS/TSS)
  return(R2)
}