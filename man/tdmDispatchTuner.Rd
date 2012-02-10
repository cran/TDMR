\name{tdmDispatchTuner}
\alias{tdmDispatchTuner}
\title{Helper function for \link{tdmCompleteEval}.}
\usage{tdmDispatchTuner(tuneMethod, confFile, spotStep, tdm, envT, dataObj)
}
\description{Helper function for \code{\link{tdmCompleteEval}}.
tdmDispatchTuner selects and starts the tuner specified by tuneMethod.}
\value{the result of the tuning algorithm.}
\seealso{\code{\link{tdmCompleteEval}}}
\author{Wolfgang Konen, FHK, Sep'2010 - Oct'2011}
\arguments{\item{tuneMethod}{the tuning algorithm given as a string. Possible values are ("spot", "lhd", "cmaes", "bfgs", "powell").}
\item{confFile}{the configuration file.}
\item{spotStep}{which step to execute for \link{SPOT} . Values "rep" and "auto" are supported by TDMR.}
\item{tdm}{the TDMR object}
\item{envT}{the environment variable}
\item{dataObj}{the object with the data set (train/vali part and test part)}
}

