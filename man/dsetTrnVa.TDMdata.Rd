\name{dsetTrnVa.TDMdata}
\alias{dsetTrnVa.TDMdata}
\title{Return the train-validation part of a \code{TDMdata} object containing the task data.}
\usage{
  \method{dsetTrnVa}{TDMdata} (x)
}
\arguments{
  \item{x}{return value from a prior call to
  \code{\link{tdmSplitTestData}}, an object of class
  \code{TDMdata}.}
}
\value{
  \code{dset}, a data frame with all train-validation
  records
}
\description{
  Return the train-validation part of a \code{TDMdata}
  object containing the task data.
}
\author{
  Wolfgang Konen, FHK, Feb'2012
}
\seealso{
  \code{\link{dsetTest.TDMdata}}
  \code{\link{tdmSplitTestData}}
}

