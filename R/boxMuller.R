#' Box-Muller transform
#'
#' boxMuller(n) returns a data frame with columns x1 and x2, representing n pairs of independent, normally-distributed random numbers
#'
#' @param n number of desired pairs
#' @return data frame with columns X1 and X2
#' @examples
#' boxMuller(500)
#' @export
#' @rdname boxMuller

boxMuller = function(n){
  X1 = rep(0, n)
  X2 = rep(0, n)
  for (m in 1:n) {
    U = stats::runif(n = 2, min = 0, max = 1)
    X1[m] = sqrt(-2*log(U[1]))*cos(2*pi*U[2])
    X2[m] = sqrt(-2*log(U[2]))*sin(2*pi*U[2])
  }
  mydata = data.frame(x = X1, y = X2)
  return(mydata)
}
