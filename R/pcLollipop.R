pcLollipop = function(x){

  P = stats::prcomp(x)
  loadings = P$x
  npc = dim(loadings)
  myplots = lapply(seq_len(ncol(loadings)), FUN = function(n){
    Load = loadings[, n]
    nL = 1:length(Load)
    p = ggplot2::ggplot(data = NULL) +
      # plot circles
      ggplot2::geom_point(aes(x = nL,
                              y = Load,
                              size = 1,
                              color = rownames(loadings))) +
      # plot lines
      ggplot2::geom_segment(aes(x = nL,
                                xend = nL,
                                y = rep(0, length(nL)),
                                yend = Load,
                                color = rownames(loadings))) +
      # labels
      ggplot2::theme(axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank()) +
      ggplot2::ylab("weight") +
      ggplot2::labs(color = "Predictor", fill = rownames(loadings)) +
      ggplot2::ggtitle(colnames(loadings)[n])
    return(p)
  })
  fig = gridExtra::grid.arrange(grobs = myplots, )
  # fig = do.call("ggarrange",
  #               c(grobs = myplots,
  #                 nrow = ncol(loadings),
  #                 common.legend = T,
  #                 legend = "right",
  #                 widths = 1,
  #                 heights = 1))
  return(fig)
}
