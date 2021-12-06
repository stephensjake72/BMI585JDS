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