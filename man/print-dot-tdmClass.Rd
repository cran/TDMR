\name{print.tdmClass}
\alias{print.tdmClass}
\title{Print an overview for a tdmClass object.}
\usage{\method{print}{tdmClass}(x, ...)
}
\description{Print an overview for a \code{tdmClass} object.}
\seealso{\code{\link{tdmClassify}}, \code{\link{print.TDMclassifier}}}
\author{Wolfgang Konen, FHK, Oct'2011 - Dec'2011}
\arguments{\item{x}{return value from a prior call to \code{\link{tdmClassify}}, an object of class \code{tdmClass}.}
\item{...}{e.g. 'type'    which information to print:
\describe{
\item{\code{"overview"}}{ (def.) relative gain on training/test set, number of records, see \code{\link{tdmClassifySummary}}}
\item{\code{"cm.train"}}{ confusion matrix on train set}
\item{\code{"cm.test"}}{ confusion matrix on test set}
\item{\code{"?"}}{ help on this method}
}}
}

