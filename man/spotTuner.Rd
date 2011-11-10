\name{spotTuner}
\alias{spotTuner}
\title{Perform a SPOT tuning.}
\usage{spotTuner(confFile, spotStep, theSpotPath, tdm, envT)
}
\description{Perform a \link{SPOT} tuning. Call \code{\link{spot}} and write the necessary information on environment \code{envT}}
\value{the result of SPOT tuning, i.e. the list \code{spotConfig}}
\arguments{\item{confFile}{task configuration for tuning algorithm}
\item{spotStep}{needed for spot}
\item{theSpotPath}{needed for spot}
\item{tdm}{the TDMR object}
\item{envT}{the environment variable}
}

