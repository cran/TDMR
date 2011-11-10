\name{print.TDMclassifier}
\alias{print.TDMclassifier}
\title{Print an overview for a TDMclassifier object.}
\usage{print.TDMclassifier(x, ...)
}
\description{Print an overview for a \code{TDMclassifier} object.}
\arguments{\item{x}{return value from a prior call to \code{\link{tdmClassifyLoop}}, an object of class \code{TDMclassifier}.}
\item{...}{e.g. 'type'    which information to print:
\describe{
\item{\code{"overview"}}{ (def.) relative gain on training/test set, number of records, see \code{\link{tdmClassifySummary}}}
\item{\code{"cm.train"}}{ confusion matrix on train set}
\item{\code{"cm.test"}}{ confusion matrix on test set}
}}
}

