\name{print.tdmRegre}
\alias{print.tdmRegre}
\title{Print an overview for a tdmRegre object.}
\usage{\method{print}{tdmRegre}(x, ...)
}
\description{Print an overview for a \code{tdmRegre} object.}
\seealso{\code{\link{tdmRegress}}, \code{\link{print.TDMregressor}}}
\author{Wolfgang Konen, FHK, Oct'2011 - Dec'2011}
\arguments{\item{x}{return value from a prior call to \code{\link{tdmRegress}}, an object of class \code{tdmRegre}.}
\item{...}{e.g. 'type'    which information to print:
\describe{
\item{\code{"overview"}}{ (def.) RMAE on training/test set, number of records, see \code{\link{tdmRegressSummary}}}
\item{\code{"..."}}{ ... other choices, TODO ...}
\item{\code{"?"}}{ help on this method}
}}
}

