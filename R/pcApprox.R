pcApprox = function(x, npc){
  # conduct centered and scaled PCA
  pcx = prcomp(x, center = T, scale. = T)
  # simplify to npc dimensions
  px = pc$x[, 1:npc]
  pr = pc$rotation[1:npc, 1:npc]
  pcs = pc$scale[1:npc]
  pcc = pc$center[1:npc]
  
  # re-center and re-scale and return the estimated data
  xApprox = t(t(px %*% t(pr))*pcs + pcc)
  
  return(xApprox)
}