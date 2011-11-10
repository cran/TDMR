\name{tdmDefaultsFill}
\alias{tdmDefaultsFill}
\title{Default values for list tdm.}
\usage{tdmDefaultsFill(tdm, mainFile, mainCommand)
}
\description{Default values for list \code{tdm}.}
\details{When called with \code{tdm = tdmDefaultsFill()}, a new list \code{tdm} is created and returned.
When called with \code{tdm = tdmDefaultsFill(mainFile="my.r",mainCommand="result<-my(opts)")}, a new list \code{tdm} is created
and returned, with the elements mainFile and mainCommand set to the specified values.
When called with \code{tdm = tdmDefaultsFill(tdm)}, an existing list \code{tdm} is filled with further default values.}
\value{\code{tdm}        the new / extended list  with default values
\item{tuneMethod}{[spot]}
\item{nExperim}{[1]}
\item{umode}{["DEF"], one out of [ "RSUB" | "CV" | "TST" | "DEF" ], see \code{\link{unbiasedBestRun_C}}}
\item{timeMode}{[1] user time}
\item{fileMode}{[TRUE]}
\item{optsVerbosity}{[0] the verbosity for the unbiased runs}
\item{theSpotPath}{[NA] use SPOT's package version}
\item{parallelCPUs}{[1]}
\item{startDir}{[getwd()]}}
\author{Wolfgang Konen, Patrick Koch, Oct'2011}
\arguments{\item{tdm}{(optional)}
\item{mainFile}{(optional) if given, create or overwrite tdm$mainFile with this value}
\item{mainCommand}{(optional) if given, create or overwrite tdm$mainCommand with this value}
}

