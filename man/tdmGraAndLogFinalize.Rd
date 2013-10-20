\name{tdmGraAndLogFinalize}
\alias{tdmGraAndLogFinalize}
\title{Finalize graphics and log file}
\usage{
  tdmGraAndLogFinalize(opts, gdObj = NULL)
}
\arguments{
  \item{opts}{with \code{opts$GD.DEVICE} one out of
  [\code{"pdf"},\code{"png"},\code{"win"},\code{"non"}]}

  \item{gdObj}{object of class TDMgdev, the return value
  from tdmGraAndLogInitialize, to ensure that
  tdmGraAndLogInitialize was called before (and the sink on
  opts$LOGFILE can be closed)}
}
\description{
  Finalize graphics and log file
}

