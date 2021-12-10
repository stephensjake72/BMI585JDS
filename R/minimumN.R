minimumN = function(d){
  test = stats::power.t.test(delta = 1, sd = 1, power = d)
  min.n = ceiling(test$n)
  return(min.n)
}
