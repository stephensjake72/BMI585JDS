chiSquareCounts = function(tib, x, y){
  mytable = tib %>%
    tabyl({{x}}, {{y}})
  xsq = table.chisq(mytable)
  return(xsq)
}