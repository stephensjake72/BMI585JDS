% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pcApprox.R
\name{pcApprox}
\alias{pcApprox}
\title{pcApprox}
\usage{
pcApprox(x, npc)
}
\arguments{
\item{x}{a data frame, tibble, or matrix}

\item{npc}{number of principal components with which to approximate x}
}
\value{
approximation of x
}
\description{
pcApprox(x, npc) returns an approximation of input data 'x' based on 'npc' principal components.
'x' can be a data frame, tibble, or matrix with columns corresponding to variables and rows to observations.
'x' need not be centered or scaled before execution.
}
\examples{
data(mtcars)
mtcarsApprox = pcApprox(mtcars, 5)
}
