welchT = function(x, y){
  mu = c(mean(x), mean(y))
  sigma = c(sd(x), sd(y))
  n = c(length(x), length(y))
  
  tstat = abs(mu[1]-mu[2])/sqrt(sum(sigma^2/n))
  return(tstat)
}