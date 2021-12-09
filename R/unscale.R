unscale = function(mat){
  # load column means and st. devs from original unscaled matrix
  mu = attributes(mat)$'scaled:center'
  sd = attributes(mat)$'scaled:scale'
  
  # multiply by st. devs and add means
  mat2 = t(t(mat)*sd + mu)
  
  return(mat2)
}
