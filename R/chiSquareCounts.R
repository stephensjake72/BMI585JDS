#' Chi-Square Counts
#'
#' chiSquareCounts(tib) returns a Chi-squared statistic for the 2x2 contingency table 'tib' by computing
#' the error between the observed and expected counts
#'
#' @param tib a 2x2 tibble, data frame, or matrix of counts
#' @return chi-square statistic
#' @examples
#' mydata = data.frame(row.names = c("Success", "Failure"), Group1 = c(21, 3), Group2 = c(14, 10))
#' chisq = chiSquareCounts(mydata)
#' @export
#' @rdname chiSquareCounts

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
