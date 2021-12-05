twoSidedZ = function(z){
  area = 2*(1 - pnorm(abs(z)))
  return(area)
}