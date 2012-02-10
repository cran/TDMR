\name{tdmClassifySummary}
\alias{tdmClassifySummary}
\title{Print summary output for result from tdmClassifiyLoop and add result$y.}
\usage{tdmClassifySummary(result, opts, dset)
}
\description{Print summary output for \code{result} from \code{tdmClassifiyLoop} and add \code{result$y}.}
\details{\code{result$y} is "minus OOB rgain" on training set for methods RF or MC.RF.
\code{result$y} is "minus rgain" on test set (=validation set) for all other methods.
\code{result$y} is the quantity which the tuner seeks to minimize.}
\value{\code{result},  an object of class \code{TDMclassifier}, with \code{result$y}, \code{result$sd.y}
(and optionally also \code{result$dset}) added}
\seealso{\code{\link{tdmClassify}}, \code{\link{tdmClassifyLoop}}, \code{\link{print.TDMclassifier}}, \code{\link{tdmRegressSummary}}}
\author{Wolfgang Konen, FHK, Sep'2010 - Oct'2011}
\arguments{\item{result}{return value from a prior call to \code{\link{tdmClassifyLoop}}, an object of class \code{TDMclassifier}.}
\item{opts}{a list from which we need here the following entries
\describe{
\item{\code{NRUN}}{ number of runs (outer loop)}
\item{\code{method}}{}
\item{\code{VERBOSE}}{}
\item{\code{dset}}{ [NULL] if !=NULL, attach it to result}
}}
\item{dset}{[NULL] if not NULL, add this data frame to the return value (may cost a lot of memory!)}
}

