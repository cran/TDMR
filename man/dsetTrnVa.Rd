% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tdmReadAndSplit.r
\name{dsetTrnVa}
\alias{dsetTrnVa}
\title{Return train-validataion data}
\usage{
dsetTrnVa(x, ...)
}
\arguments{
\item{x}{return value from a prior call to \code{\link{tdmReadAndSplit}}, an object of class \code{\link{TDMdata}}.}

\item{...}{may contain nExp, experiment number}
}
\value{
\code{dset}, a data frame with all train-validation records
}
\description{
Return train-validataion data for an object.
See \code{\link{dsetTrnVa.TDMdata}} for details.
}
\seealso{
\code{\link{dsetTrnVa.TDMdata}}   \code{\link{dsetTest.TDMdata}}
}
\keyword{internal}
