\name{lhdTuner}
\alias{lhdTuner}
\title{Perform LHD tuning.}
\usage{lhdTuner(confFile, spotStep, theSpotPath, tdm, envT)
}
\description{Perform LHD tuning. Perform a parameter tuning using a Latin hypercube design (LHD) 
for obtaining best design points.}
\details{Configure SPOT so that all the budget is used for the initial design (usually LHD)}
\value{the result of LHD sampling, i.e. the list \code{spotConfig}}
\arguments{\item{confFile}{task configuration for tuning algorithm}
\item{spotStep}{needed for spot}
\item{theSpotPath}{needed for spot}
\item{tdm}{the TDMR object}
\item{envT}{the environment variable}
}

