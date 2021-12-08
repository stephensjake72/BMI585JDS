unscale = function(mat){
  # load column means and st. devs from original unscaled matrix
  mu = attributes(mat)$col.means
  sd = attributes(mat)$col.sds
  
  # unscale
  # apply st. dev. scaling row-wise so dimensions match
  mat = apply(mat, 1, FUN = function(x){x*sd})
  # mat is now a matrix with variables as rows, so apply mean scaling column-wise
  mat = apply(mat, 2, FUN = function(x){x + mu})
  
  # transpose mat to revert back to column variables
  mat = t(mat)
  
  return(mat)
}
