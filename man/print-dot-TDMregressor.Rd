\name{print.TDMregressor}
\alias{print.TDMregressor}
\title{Print an overview for a TDMregressor object.}
\usage{\method{print}{TDMregressor}(x, ...)
}
\description{Print an overview for a \code{TDMregressor} object.}
\seealso{\code{\link{tdmRegressLoop}}, \code{\link{print.tdmRegre}}}
\arguments{\item{x}{return value from a prior call to \code{\link{tdmRegressLoop}}, an object of class \code{TDMregressor}.}
\item{...}{e.g. 'type'    which information to print:
\describe{
\item{\code{"overview"}}{ (def.) RMAE on training/test set, number of records, see \code{\link{tdmRegressSummary}}}
\item{\code{"..."}}{ ... other choices, TODO ...}
}}
}

