\name{dsetTest.TDMdata}
\alias{dsetTest.TDMdata}
\title{Return the test part of a \code{TDMdata} object containing the task data.}
\usage{
  \method{dsetTest}{TDMdata} (x)
}
\arguments{
  \item{x}{return value from a prior call to
  \code{\link{tdmSplitTestData}}, an object of class
  \code{TDMdata}.}
}
\value{
  \code{tset}, a data frame with all test records
}
\description{
  Return the test part of a \code{TDMdata} object
  containing the task data.
}
\author{
  Wolfgang Konen, FHK, Feb'2012
}
\seealso{
  \code{\link{unbiasedRun}} \code{\link{dsetTrnVa.TDMdata}}
}

