#' Welch's T
#'
#' welchT(x, y) takes numerical vectors 'x' and 'y' and returns a Welch's t-statistic between the two groups.
#'
#' @param x a numerical vector
#' @param y a numerical vector
#' @return Welch's t-statistic
#' @examples
#' x = rnorm(n = 50, mean = 0, sd = 1)
#' y = rnorm(n = 50, mean = 0, sd = 2.5)
#' t.stat = welchT(x, y)
#' @export
#' @rdname welchT

welchT = function(x, y){
  mu = c(mean(x), mean(y))
  sigma = c(sd(x), sd(y))
  n = c(length(x), length(y))

  tstat = abs(mu[1]-mu[2])/sqrt(sum(sigma^2/n))
  return(tstat)
}
