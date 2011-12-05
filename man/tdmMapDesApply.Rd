\name{tdmMapDesApply}
\alias{tdmMapDesApply}
\title{Apply the mapping from des to opts.}
\usage{tdmMapDesApply(des, opts, k, envT, tdm)
}
\description{Apply the mapping from \code{des} to \code{opts}.     
For each variable which appears in .roi (and thus in .des file and design point data frame \code{des}): 
set its counterpart in list \code{opts} to the values of the \code{k}-th row in \code{des}.
For each variable not appearing: leave its counterpart in \code{opts} at its default value from .apd file.}
\value{\code{opts}, the modified list of options}
\seealso{\code{\link{tdmMapDesLoad}}}
\arguments{\item{des}{design points data frame}
\item{opts}{list of options}
\item{k}{apply mapping for the \code{k}-th design point}
\item{envT}{environment, we needed here \code{envT$map} and \code{envT$mapUser}, see \code{\link{tdmMapDesLoad}}}
\item{tdm}{list}
}
