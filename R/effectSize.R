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
