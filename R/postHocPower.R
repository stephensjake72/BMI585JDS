#' postHocPower
#'
#' postHocPower(d, n1, n2) returns an estimate of the post-hoc power of 1000 simulated trials with 2 groups with observations n1 and n2 and a Cohen's d of 'd'.
#' @param d desired Cohen's d
#' @param n1 number of observations in group 1
#' @param n2 number of observations in group 2
#' @return power estimate
#' @examples
#' n1 = 100
#' n2 = 150
#' d = 3
#' php = postHocPower(d, n1, n2)
#' @export
#' @rdname postHocPower

postHocPower = function(d, n1, n2){
  p = rep(0, 1000)
  for (i in 1:length(p)){
    group1 = rnorm(n1)
    group2 = rnorm(n2) + d

    test = power.t.test(delta = mean(group1) - mean(group2),
                        sd = sqrt((var(group1) + var(group2))/2),
                        n = (n1 + n2)/2)
    p[i] = test$power
  }
  return(mean(p))
}
