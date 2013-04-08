\name{tdmEnvTMakeNew}
\alias{TDMenvir}
\alias{tdmEnvTMakeNew}
\title{Construct a new environment envT of class \code{\link{TDMenvir}}.}
\usage{
  tdmEnvTMakeNew(tdm = NULL)
}
\arguments{
  \item{tdm}{a list with general settings for TDMR, see
  \code{\link{tdmDefaultsFill}}}
}
\value{
  Environment \code{envT}, an object of class
  \code{\link{TDMenvir}}, containing (among others) the
  elements \item{\code{runList}}{ \code{=tdm$runList} }
  \item{\code{spotList}}{ \code{=tdm$spotList} }
  \item{\code{tdm}}{ \code{=tdm} } \item{\code{getBst}}{
  accessor function(confFile,nExp,theTuner) into
  \code{envT$bstGrid} } \item{\code{getRes}}{ accessor
  function(confFile,nExp,theTuner) into \code{envT$resGrid}
  } \item{\code{sCList}}{ list of spotConfig-objects, as
  many as \code{envT$runList} has elements. Each spotConfig
  object sCList[[k]] contains a list \code{opts} as
  element, which is read from .apd file specified in
  \code{envT$runList[k]}.  }
}
\description{
  Given the general TDMR settings in \code{tdm}, construct
  an appropriate environment \code{envT}. This is needed as
  input for \code{\link{tdmBigLoop}}.
}
\author{
  Wolfgang Konen (\email{wolfgang.konen@fh-koeln.de}),
  Patrick Koch
}
\seealso{
  \code{\link{tdmBigLoop}}
}

