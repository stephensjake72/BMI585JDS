bhAdjust = function(p, alpha){
  # make rank vector
  rank = order(p)
  # number of tests
  m = length(p)
  # create vector of FALSE the length of p
  sig.p = rep(FALSE, m)
  # initialize n
  n = 1
  # iterate through p values until one is greater than the corrected alpha
  check.true = TRUE
  while (check.true & n <= m){
    # find the next entry in ascending order
    id = which(rank == n)
    # check adjusted significance
    if (p[id] < alpha/(m-n+1)){
      sig.p[id] = TRUE
      n = n + 1
    } else {
      check.true = FALSE
    }
  }
  return(sig.p)
}