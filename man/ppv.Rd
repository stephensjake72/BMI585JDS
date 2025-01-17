% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppv.R
\name{ppv}
\alias{ppv}
\title{Positive Predictive Value}
\usage{
ppv(pred, truth)
}
\arguments{
\item{pred}{a binary or logical vector of predicted classes}

\item{truth}{a binary or logical vector of observed classes}
}
\value{
positive predictive value
}
\description{
ppv(pred, truth) return the positive predictive value of predicted classes 'pred' and observed classes 'truth'.
PPV is computed as the number of correct positive classifications divided by the sum of correct positive and false positive predictions.
}
\examples{
pred = c(1, 1, 1, 0, 0, 0)
truth = c(1, 0, 1, 0, 0, 0)
ppv(pred, truth)

pred = c(T, T, T, F, F, F)
truth = c(T, F, T, F, F, F)
ppv(pred, truth)

}
