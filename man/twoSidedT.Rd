% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/twoSidedT.R
\name{twoSidedT}
\alias{twoSidedT}
\title{Two-Sided T}
\usage{
twoSidedT(t, n)
}
\arguments{
\item{t}{a test-statistic}

\item{n}{number of degrees of freedom}
}
\value{
area
}
\description{
twoSidedT(t, n) takes a test-statistic 't' and degrees of freedom 'n' and returns the area under the t-distribution >= |t|
}
\examples{
t = 3
n = 5
area = twoSidedT(t, n)
}
