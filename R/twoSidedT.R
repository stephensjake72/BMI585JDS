#' Two-Sided T
#'
#' twoSidedT(t, n) takes a test-statistic 't' and degrees of freedom 'n' and returns the area under the t-distribution >= |t|
#'
#' @param t a test-statistic
#' @param n number of degrees of freedom
#' @return area
#' @examples
#' t = 3
#' n = 5
#' area = twoSidedT(t, n)
#' @export
#' @rdname twoSidedT

twoSidedT = function(t, n){
  area = 2*(1 - pt(abs(t), n))
  return(area)
}
