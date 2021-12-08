chiSquareCounts = function(tib){
  
  # extract entries from contingency table
  a = contingency[1, 1]
  b = contingency[1, 2]
  c = contingency[2, 1]
  d = contingency[2, 2]
  
  # compute expected values table
  expected = contingency
  expected[1, 1] = (a + c)*(a + b)/sum(contingency)
  expected[2, 1] = (a + c)*(1 - (a + b)/sum(contingency))
  expected[1, 2] = (b + d)*(a + b)/sum(contingency)
  expected[2, 2] = (b + d)*(1 - (a + b)/sum(contingency))
  
  # compute chi-square statistic based on observed and expected values
  chisq.stat = sum((contingency - expected)^2/expected)
  
  # return the chi-square statistic
  return(chisq.stat)
}