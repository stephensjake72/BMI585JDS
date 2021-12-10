#' Bonferroni-Holm Adjustment
#'
#' bhAdjust(p, alpha) takes a vector of p-values p and a significance level alpha and returns a logical vector
#' indicating which p-values meet the significance level under the Bonferroni-Holm adjustment criteria
#'
#' @param p a vector of p-values
#' @param alpha a desired significance level, conventionally .05
#' @return sig.p, a logical vector indicating p-values met significance criteria
#' @examples
#' p = c(0.0001, .00025, .0005, .00075, .001, .005)
#' alpha = .05
#' bhAdjust(p, alpha)
#' @export
#' @rdname bhAdjust


bhAdjust = function(p, alpha){
  # make rank vector
  rank = order(p)
  # number of tests
  m = length(p)
  # create vector of FALSE the length of p
  sig.p = rep(FALSE, m)
  # initialize n
  n = 1
  # iterate through p values until one is greater than the corrected alpha
  check.true = TRUE
  while (check.true & n <= m){
    # find the next entry in ascending order
    id = which(rank == n)
    # check adjusted significance
    if (p[id] < alpha/(m-n+1)){
      sig.p[id] = TRUE
      n = n + 1
    } else {
      check.true = FALSE
    }
  }
  return(sig.p)
}
