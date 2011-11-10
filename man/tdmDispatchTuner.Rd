\name{tdmDispatchTuner}
\alias{tdmDispatchTuner}
\title{tdmDispatchTuner:...}
\usage{tdmDispatchTuner(tuneMethod, confFile, spotStep, tdm, envT)
}
\description{tdmDispatchTuner:
helper function for tdmCompleteEval:
select and start the tuner specified by tuneMethod}
\value{the result of the tuning algorithm.}
\arguments{\item{tuneMethod}{the tuning algorithm given as a char. Possible options are ("spot", "lhd", "cmaes", "bfgs", "powell").}
\item{confFile}{the configuration file.}
\item{spotStep}{step for spot. At the moment "rep" and "auto" are supported by TDMR.}
\item{tdm}{The TDMR object}
\item{envT}{The environment variable.}
}

