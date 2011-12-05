\name{tdmStartSpot}
\alias{tdmStartSpot}
\title{Start a tuning evaluation of a DM task for SPOT tuner...}
\usage{tdmStartSpot(spotConfig)
}
\description{Start a tuning evaluation of a DM task for \link{SPOT} tuner}
\details{This function is called by \code{\link{spot}} for tuning evaluations. It accumulates in 
\code{spotConfig$alg.currentResult} the RES data frame of all evaluations and in 
\code{spotConfig$alg.currentBest} the BST data frame of the so far best solution.

\code{\link{spot}} is invoked from \code{\link{spotTuner}} or \code{\link{lhdTuner}}. 
The latter is simply a \link{SPOT}-run with all budget devoted to the initial design.}
\value{spotConfig}
\seealso{\code{\link{spot}}, \code{\link{spotTuner}}, \code{\link{lhdTuner}}, \code{\link{tdmDispatchTuner}}}
\author{Wolfgang Konen}
\arguments{\item{spotConfig}{the list of configurations for \link{SPOT}. Besides the usual \link{SPOT} settings,
this list has to contain an element \code{tdm} with the mandatory elements \itemize{
\item tdm$mainFile:     the R file of the DM task to source
\item tdm$mainCommand:  the R command to execute, usually \code{result <- main_TASK(opts)}. 
(It is expected that \code{mainCommand} returns \code{result} and that the element 
\code{result$y} contains the quantity to be minimized by \link{SPOT}.)
}}
}

