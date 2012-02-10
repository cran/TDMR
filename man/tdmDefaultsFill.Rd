\name{tdmDefaultsFill}
\alias{tdmDefaultsFill}
\title{Default values for list tdm.}
\usage{tdmDefaultsFill(tdm, mainFile, mainCommand)
}
\description{Default values for list \code{tdm}.  This list controls the tuning and unbiased evaluation phase.}
\details{When called with \code{tdm = tdmDefaultsFill()}, a new list \code{tdm} is created and returned.
When called with \code{tdm = tdmDefaultsFill(mainFile="my.r",mainCommand="result<-my(opts)")}, a new list \code{tdm} is created
and returned, with the elements mainFile and mainCommand set to the specified values.
When called with \code{tdm = tdmDefaultsFill(tdm)}, an existing list \code{tdm} is filled with further default values.
If tdm$mainFunc is missing, but tdm$mainFile exists, then tdmDefaultsFill will set tdm$mainFunc=sub(".r","",basename(tdm$mainFile),fixed=TRUE).}
\value{\code{tdm}     the new / extended list,  where additional elements, if they are not yet def'd,  are set as: 
\item{mainFunc}{\code{sub(".r","",basename(tdm$mainFile),fixed=TRUE)}, if tdm$mainFile is set, else \code{"mainFunc"}}
\item{mainCommand}{\code{result <- tdm$mainFunc(opts)}}
\item{unbiasedFunc}{["unbiasedRun"] which function to call for unbiased evaluation}
\item{tuneMethod}{["spot"] other choices: "cmaes", "bfgs", ..., see \code{\link{tdmDispatchTuner}} }
\item{nExperim}{[1]}
\item{umode}{["RSUB"], one out of [ "RSUB" | "CV" | "TST" | "SP_T" ], see \code{\link{unbiasedRun}}}
\item{timeMode}{[1] 1: proc time, 2: elapsed time}
\item{fileMode}{[TRUE]}
\item{experFile}{[NULL] where to append final results  }
\item{theSpotPath}{[NA] use SPOT's package version}
\item{parallelCPUs}{[1] 1: sequential, >1: parallel with snowFall and this many cpus}
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

