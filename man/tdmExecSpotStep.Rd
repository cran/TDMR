\name{tdmExecSpotStep}
\alias{tdmExecSpotStep}
\title{Execute the tuning process with the given \code{spotStep}}
\usage{
  tdmExecSpotStep(tdm, spotStep)
}
\arguments{
  \item{tdm}{a list with general settings for TDMR, see
  \code{\link{tdmDefaultsFill}}}

  \item{spotStep}{a string, either "auto", "rep" or
  "report"}
}
\value{
  Environment \code{envT} containing (among others) the
  elements \item{\code{runList}}{ \code{=tdm$runList} }
  \item{\code{spotList}}{ \code{=tdm$spotList} }
  \item{\code{tdm}}{ \code{=tdm} } \item{\code{getBst}}{
  accessor function(confFile,nExp,theTuner) into
  \code{envT$bstGrid} } \item{\code{getRes}}{ accessor
  function(confFile,nExp,theTuner) into \code{envT$resGrid}
  } \item{\code{sCList}}{ list of spotConfig objects, as
  many as \code{envT$runList} has elements. Each spotConfig
  object \code{sCList[[k]]} contains a list \code{opts} as
  element, which is read from .apd file specified in
  \code{envT$runList[k]}.  }
}
\description{
  If \code{spotStep=="auto"}, construct \code{envT} from
  the general TDMR settings in \code{tdm}.\cr If
  \code{spotStep=="rep"} or \code{"report"}, load
  \code{envT} from file \code{tdm$filenameEnvT} (from a
  prior tuning run).\cr Then call
  \code{\link{tdmBigLoop}(envT,spotStep)}.
}
\details{
  If \code{tdm$filenameEnvT} is NULL, set it to
  \code{sub(".conf",".RData",tdm$runList[1])}.
}
\author{
  Wolfgang Konen (\email{wolfgang.konen@fh-koeln.de})
}
\seealso{
  \code{\link{tdmBigLoop}}, \code{\link{tdmEnvTMakeNew}}
}

