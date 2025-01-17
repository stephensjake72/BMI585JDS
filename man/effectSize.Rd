% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effectSize.R
\name{effectSize}
\alias{effectSize}
\title{Effect Size}
\usage{
effectSize(x, g)
}
\arguments{
\item{x}{a vector of observed data}

\item{g}{a grouping vector}
}
\value{
effect size
}
\description{
effectSize(x, g) takes the data vector 'x' and the group vector 'g' and returns the effect size between the two groups
}
\examples{
groupa = rnorm(n = 50, mean = 0, sd = 1)
groupb = rnorm(n = 50, mean = 2, sd = 1)
x = c(groupa, groupb)
g = rep(c("A", "B"), each = 50)
effect.size = effectSize(x, g)
}
