\name{tdmMapDesApply}
\alias{tdmMapDesApply}
\title{Apply the mapping from des to opts.}
\usage{tdmMapDesApply(des, opts, k, envT, tdm)
}
\description{Apply the mapping from \code{des} to \code{opts}.     
For each variable which appears in .roi (and thus in .des file): set its counterpart in list \code{opts}.
For each variable not appearing: leave its counterpart in \code{opts} at its default value from .apd file.}
\value{opts modified list of options}
\arguments{\item{des}{design points data frame}
\item{opts}{list of options}
\item{k}{apply mapping for the \code{k}th design point}
\item{envT}{environment}
\item{tdm}{list}
}

