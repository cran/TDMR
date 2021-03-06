% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tdmReadAndSplit.r
\name{dsetTrnVa.TDMdata}
\alias{dsetTrnVa.TDMdata}
\title{Return train-validation data of \code{\link{TDMdata}} object}
\usage{
\method{dsetTrnVa}{TDMdata}(x, ...)
}
\arguments{
\item{x}{return value from a prior call to \code{\link{tdmReadAndSplit}}, an object of class \code{\link{TDMdata}}.}

\item{...}{may contain nExp, experiment number, needed only if \code{x$tdm$umode=="SP_T"}: 
add nExp to seed when randomly splitting in train and test data [default: nExp=0]}
}
\value{
\code{dset}, a data frame with all train-validation records
}
\description{
Return the train-validation part of a \code{\link{TDMdata}} object containing the task data.
}
\seealso{
\code{\link{dsetTest.TDMdata}} \code{\link{tdmReadAndSplit}}
}
\author{
Wolfgang Konen, THK
}
