\name{print.TDMregressor}
\alias{print.TDMregressor}
\title{Print an overview for a TDMregressor object.}
\usage{print.TDMregressor(x, ...)
}
\description{Print an overview for a \code{TDMregressor} object.}
\arguments{\item{x}{return value from a prior call to \code{\link{tdmClassifyLoop}}, an object of class \code{TDMclassifier}.}
\item{...}{e.g. 'type'    which information to print:
\describe{
\item{\code{"overview"}}{ (def.) RMAE on training/test set, number of records, see \code{\link{tdmRegressSummary}}}
\item{\code{"..."}}{ ... other choices, TODO ...}
}}
}

