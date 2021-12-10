chiSquareCounts = function(tib){

  # extract entries from contingency table
  a = tib[1, 1]
  b = tib[1, 2]
  c = tib[2, 1]
  d = tib[2, 2]

  # compute expected values table
  expected = tib
  expected[1, 1] = (a + c)*(a + b)/sum(tib)
  expected[2, 1] = (a + c)*(1 - (a + b)/sum(tib))
  expected[1, 2] = (b + d)*(a + b)/sum(tib)
  expected[2, 2] = (b + d)*(1 - (a + b)/sum(tib))

  # compute chi-square statistic based on observed and expected values
  chisq.stat = sum((tib - expected)^2/expected)

  # return the chi-square statistic
  return(chisq.stat)
}
