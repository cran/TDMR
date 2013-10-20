\name{tdmEnvTReadApd}
\alias{tdmEnvTReadApd}
\title{Re-read APD-files (list opts) for an environment envT of class \code{\link{TDMenvir}}.}
\usage{
  tdmEnvTReadApd(envT, tdm)
}
\arguments{
  \item{envT}{an environment of class
  \code{\link{TDMenvir}}}

  \item{tdm}{a list with general settings for TDMR (can be
  envT$tdm)}
}
\value{
  modified environment \code{envT}, an object of class
  \code{\link{TDMenvir}}, where the items
  \item{\code{sCList[[k]]$opts}}{ \code{=tdm$runList} } are
  modefied
}
\description{
  Given an already existing envT with envT$sClist filled,
  re-read the opts settings from APD files whose filenames
  are specified in envT$sClist[[k]]$io.apdfilename.
}
\author{
  Wolfgang Konen (\email{wolfgang.konen@fh-koeln.de}),
  Patrick Koch
}
\seealso{
  \code{\link{tdmEnvTMakeNew}}
}

