#' Minimum N
#'
#' minimumN(d) maps a desired power 'd' to a minimum number of observations needed to achieve the desired  power based on a t-test
#' with assumed Cohen's d of 1
#'
#' @param d desired power
#' @return minimum number of observations
#' @examples
#' minimumN(.8)
#' @export
#' @rdname minimumN

minimumN = function(d){
  test = stats::power.t.test(delta = 1, sd = 1, power = d)
  min.n = ceiling(test$n)
  return(min.n)
}
