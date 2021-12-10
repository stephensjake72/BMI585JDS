#' FDR Adjustment
#'
#' fdrAdjust(p, alpha) takes a vector of p-values p and a significance level alpha and returns a logical vector
#' indicating which p-values meet the significance level under the False Discovery Rate adjustment criteria
#'
#' @param p a vector of p-values
#' @param alpha a desired significance level, conventionally .05
#' @return sig.p, a logical vector indicating p-values met significance criteria
#' @examples
#' p = c(0.0001, .00025, .0005, .00075, .001, .005)
#' alpha = .05
#' fdrAdjust(p, alpha)
#' @export
#' @rdname fdrAdjust

fdrAdjust = function(p, alpha){
  rank = order(p)
  m = length(p)
  # create vector of FALSE the length of p
  sig.p = rep(FALSE, m)
  # initialize k
  k = 1
  # iterate through p values until a p value is over the corrected alpha
  check.true = TRUE
  while (check.true & k <= m){
    id = which(rank == k)
    if (p[id] <= k*alpha/m){
      sig.p[id] = TRUE
      k = k + 1
    } else {
      check.true = FALSE
    }
  }
  return(sig.p)
}
