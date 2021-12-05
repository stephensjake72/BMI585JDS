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