\name{tdmOptsDefaultsFill}
\alias{tdmOptsDefaultsFill}
\title{Fill the current opts.}
\usage{tdmOptsDefaultsFill(opts, suffix=".csv")
}
\description{Fill the current \code{opts}. Fill the current \code{opts} with further default 
parameters if they are not yet defined. The defaults may depend on previously 
defined elements of \code{opts} (e.g. \code{opts$filename}).}
\value{opts   the options}
\arguments{\item{opts}{the options}
\item{suffix}{the suffix of \code{opts$filename}. If NULL, take opts$filesuffix}
}

