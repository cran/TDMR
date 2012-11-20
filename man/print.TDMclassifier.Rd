\name{print.TDMclassifier}
\alias{print.tdmClass}
\alias{print.TDMclassifier}
\title{Print an overview for a \code{\link{TDMclassifier}} or \code{tdmClass} object.}
\usage{
  \method{print}{TDMclassifier} (x, ...)

  \method{print}{tdmClass} (x, ...)
}
\arguments{
  \item{x}{an object of class \code{tdmClass}, as returned
  from a prior call to \code{\link{tdmClassify}}, \cr or an
  object of class \code{\link{TDMclassifier}}, as returned
  from a prior call to \code{\link{tdmClassifyLoop}}.}

  \item{...}{e.g. 'type' which information to print:
  \describe{ \item{\code{"overview"}}{ (default) relative
  gain on training/test set, number of records, see
  \code{\link{tdmClassifySummary}}}
  \item{\code{"cm.train"}}{ confusion matrix on train set}
  \item{\code{"cm.vali"}}{ confusion matrix on test set}
  \item{\code{"?"}}{ help on this method} }}
}
\description{
  Print an overview for a \code{\link{TDMclassifier}} or
  \code{tdmClass} object.
}
\author{
  Wolfgang Konen, FHK, Oct'2011 - Dec'2011
}
\seealso{
  \code{\link{tdmClassify}}, \code{\link{TDMclassifier}}
}

