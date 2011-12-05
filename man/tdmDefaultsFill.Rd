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
\value{\code{tdm}     the new / extended list,  where additional elements, if they are not yet def'd,  are set as: 
\item{tuneMethod}{[spot]}
\item{nExperim}{[1]}
\item{umode}{["DEF"], one out of [ "RSUB" | "CV" | "TST" | "DEF" ], see \code{\link{unbiasedRun}}}
\item{timeMode}{[1] user time}
\item{fileMode}{[TRUE]}
\item{theSpotPath}{[NA] use SPOT's package version}
\item{parallelCPUs}{[1]}
\item{startDir}{[getwd()]}
\item{test2.string}{["default cutoff"] }
\item{optsVerbosity}{[0] the verbosity for the unbiased runs}
\item{nrun}{[5] number of runs for unbiased runs}
\item{tstFrac}{[0.2] test set fraction for unbiased runs (only for umode="RSUB") }
\item{tstCol}{["TST"] opts$TST.COL for unbiased runs (only for umode="TST") }
\item{nfold}{[10] number of CV-folds for unbiased runs (only for umode="CV") }}
\author{Wolfgang Konen, Patrick Koch, Oct'2011}
\arguments{\item{tdm}{(optional)}
\item{mainFile}{(optional) if given, create or overwrite tdm$mainFile with this value}
\item{mainCommand}{(optional) if given, create or overwrite tdm$mainCommand with this value}
}

