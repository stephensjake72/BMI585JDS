#' pcApprox
#'
#' pcApprox(x, npc) returns an approximation of input data 'x' based on 'npc' principal components.
#' 'x' can be a data frame, tibble, or matrix with columns corresponding to variables and rows to observations.
#' 'x' need not be centered or scaled before execution.
#'
#' @param x a data frame, tibble, or matrix
#' @param npc number of principal components with which to approximate x
#' @return approximation of x
#' @examples
#' data(mtcars)
#' mtcarsApprox = pcApprox(mtcars, 5)
#' @export
#' @rdname pcApprox

pcApprox = function(x, npc){
  # conduct centered and scaled PCA
  pcx = stats::prcomp(x, center = T, scale. = T)
  # simplify to npc dimensions
  px = pc$x[, 1:npc]
  pr = pc$rotation[1:npc, 1:npc]
  pcs = pc$scale[1:npc]
  pcc = pc$center[1:npc]

  # re-center and re-scale and return the estimated data
  xApprox = t(t(px %*% t(pr))*pcs + pcc)

  return(xApprox)
}
