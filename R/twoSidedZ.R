#' Two-Sided Z
#'
#' twoSidedZ(z) takes a test-statistic 'z' and returns the area under the normal distribution >= |z|
#'
#' @param z a test-statistic
#' @return area
#' @examples
#' z = 4
#' area = twoSidedZ(z)
#' @export
#' @rdname twoSidedZ

twoSidedZ = function(z){
  area = 2*(1 - pnorm(abs(z)))
  return(area)
}
