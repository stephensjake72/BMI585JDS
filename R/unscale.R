#' Unscale
#'
#' unscale(mat) takes a scaled object input and rescales and recenters data to its original mean and standard deviation.
#'
#' @param mat a scaled object
#' @return unscaled data
#' @examples
#' mat = mtcars
#' mat.scaled = scale(mat)
#' mat.unscaled = unscale(mat.scaled)
#'
#' @export
#' @rdname unscale

unscale = function(mat){
  # load column means and st. devs from original unscaled matrix
  mu = attributes(mat)$'scaled:center'
  sd = attributes(mat)$'scaled:scale'

  # multiply by st. devs and add means
  mat2 = t(t(mat)*sd + mu)

  return(mat2)
}
