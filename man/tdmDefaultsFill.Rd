\name{tdmDefaultsFill}
\alias{tdmDefaultsFill}
\title{Default values for list \code{tdm}.  This list controls the tuning and unbiased evaluation phase.}
\usage{
  tdmDefaultsFill(tdm = NULL, mainFile = NULL)
}
\arguments{
  \item{tdm}{(optional)}

  \item{mainFile}{(optional) if given, create or overwrite
  tdm$mainFile with this value}
}
\value{
  \code{tdm} the new / extended list, where additional
  elements, if they are not yet def'd, are set as:
  \item{mainFile}{[NULL] if not NULL, source this file from
  the current dir. It should contain the definition of
  tdm$mainFunc.  }
  \item{mainFunc}{\code{sub(".r","",basename(tdm$mainFile),fixed=TRUE)},
  if tdm$mainFile is set and tdm$mainFunc is NULL, else
  \code{"mainFunc"} This is the name of the function called
  in \code{\link{tdmStartSpot}} and
  \code{\link{unbiasedRun}} }
  \item{unbiasedFunc}{["unbiasedRun"] which function to
  call for unbiased evaluation} \item{tuneMethod}{["spot"]
  other choices: "cmaes", "bfgs", ..., see
  \code{\link{tdmDispatchTuner}} } \item{nExperim}{[1]}
  \item{umode}{["RSUB"], one out of [ "RSUB" | "CV" | "TST"
  | "SP_T" ], see \code{\link{unbiasedRun}}}
  \item{timeMode}{[1] 1: proc time, 2: system time, 3:
  elapsed time (columns \code{Time.TST} and \code{Time.TRN}
  in \code{envT$theFinals}} \item{fileMode}{[FALSE]}: see
  "Note" section in \code{\link{tdmCompleteEval}} and
  \code{\link{tdmBigLoop}} \item{finalFile}{[NULL] filename
  where to save \code{envT$theFinals}, only relevant for
  \code{tdm$fileMode==TRUE}} \item{experFile}{[NULL]
  filename where to append \code{envT$theFinals}, only
  relevant for \code{tdm$fileMode==TRUE} }
  \item{theSpotPath}{[NA] use SPOT's package version}
  \item{parallelCPUs}{[1] 1: sequential, >1: parallel with
  snowFall and this many CPUs} \item{parallelFuncs}{[NULL]
  in case tdm$parallelCPUs>1: a string vector with
  functions which are sfExport'ed in addition to
  tdm$mainFunc.  } \item{path}{[getwd()] where to search
  .conf and .apd file} \item{stratified}{[NULL] see
  \code{\link{tdmSplitTestData}} } \item{tdmPath}{[NULL]
  from where to source the R sources. If NULL load library
  TDMR instead.  } \item{test2.string}{["default cutoff"] }
  \item{optsVerbosity}{[0] the verbosity for the unbiased
  runs} \item{nrun}{[5] number of runs for unbiased runs}
  \item{tstFrac}{[0.2] test set fraction for unbiased runs
  (only for umode="RSUB") } \item{tstCol}{["TST"]
  opts$TST.COL for unbiased runs (only for umode="TST") }
  \item{nfold}{[10] number of CV-folds for unbiased runs
  (only for umode="CV") } \item{TST.trnFrac}{[NULL] train
  set fraction, copied to opts$TST.trnFrac if not NULL. }
  \item{TST.valiFrac}{[NULL] validation set fraction,
  copied to opts$TST.valiFrac if not NULL. }
}
\description{
  When called with \code{tdm = tdmDefaultsFill()}, a new
  list \code{tdm} is created and returned.  When called
  with \code{tdm = tdmDefaultsFill(mainFile="my.r"}, a new
  list \code{tdm} is created and returned, with the element
  mainFile set to the specified value.  When called with
  \code{tdm = tdmDefaultsFill(tdm)}, an existing list
  \code{tdm} is filled with further default values.  If
  tdm$mainFunc is missing, but tdm$mainFile exists, then
  tdmDefaultsFill will set
  tdm$mainFunc=sub(".r","",basename(tdm$mainFile),fixed=TRUE).
}
\note{
  For \code{tdm$timeMode}, the 'user time' is the CPU time
  charged for the execution of user instructions of the
  calling process.  The 'system time' is the CPU time
  charged for execution by the system on behalf of the
  calling process.  The 'elapsed time' is the 'real'
  (wall-clock) time since the process was started.
}
\author{
  Wolfgang Konen, Patrick Koch, Oct'2011
}

