#' Effect Size
#'
#' effectSize(x, g) takes the data vector 'x' and the group vector 'g' and returns the effect size between the two groups
#'
#' @param x a vector of observed data
#' @param g a grouping vector
#' @return effect size
#' @examples
#' groupa = rnorm(n = 50, mean = 0, sd = 1)
#' groupb = rnorm(n = 50, mean = 2, sd = 1)
#' x = c(groupa, groupb)
#' g = rep(c("A", "B"), each = 50)
#' effect.size = effectSize(x, g)
#' @export
#' @rdname effectSize
#'

effectSize = function(x, g){
  groups = unique(g)
  x1 = x[g == groups[1]]
  x0 = x[g == groups[2]]

  mu1 = mean(x1)
  mu0 = mean(x0)

  S = stats::sd(x)

  d = abs(mu1 - mu0)/S
  return(d)
}
