\name{tdmGraAndLogInitialize}
\alias{tdmGraAndLogInitialize}
\title{Initialize graphics and log file.}
\usage{tdmGraAndLogInitialize(opts)
}
\description{Initialize graphics and log file.}
\details{The log file is opened in \code{opts$dir.output/opts$LOGFILE}.}
\value{\code{gdObj}, an object of class TDMgdev. Pass this object on when calling tdmGraAndLogFinalize(opts,gdObj)
(if not, a warning is issued before the sink-closing-error occurs)}
\arguments{\item{opts}{with \code{opts$GD.DEVICE} one out of [\code{"pdf"},\code{"png"},\code{"win"},\code{"non"}]}
}

