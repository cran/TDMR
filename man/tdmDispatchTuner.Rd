\name{tdmDispatchTuner}
\alias{tdmDispatchTuner}
\title{Helper function for \code{\link{tdmBigLoop}}.}
\usage{
  tdmDispatchTuner(tuneMethod, confFile, spotStep, tdm,
    envT, dataObj)
}
\arguments{
  \item{tuneMethod}{the tuning algorithm given as a string.
  Possible values are \{ "spot" | "lhd" | "cmaes" | "cma_j"
  | "bfgs" | "powell" \}.}

  \item{confFile}{the configuration file.}

  \item{spotStep}{which step to execute for \link{SPOT} .
  Values "rep" ("report") and "auto" are supported by
  TDMR.}

  \item{tdm}{the TDMR object}

  \item{envT}{the environment variable}

  \item{dataObj}{the \code{\link{TDMdata}} object
  containing the data set (train/vali part and test part)}
}
\value{
  The result \code{tunerVal} of the tuning algorithm, this
  is the list envT$spotConfig, extended by
  \item{\code{alg.currentResult}}{ the RES data frame }
  \item{\code{alg.currentBest}}{ the BST data frame }
  \item{...}{[optional] further results from the tuning
  algorithm \code{tuneMethod} }
}
\description{
  tdmDispatchTuner selects and starts the tuner specified
  by tuneMethod. \cr See the 'Details' section of
  \code{\link{tdmBigLoop}} for a list of available tuners.
}
\note{
  Side effects: Environment \code{envT} is extended by
  \itemize{ \item \code{envT$tunerVal = tunerVal} \item
  \code{envT$res } the RES data frame \item \code{envT$bst
  } the BST data frame }
}
\author{
  Wolfgang Konen, FHK, Sep'2010 - Oct'2011
}
\seealso{
  \code{\link{tdmBigLoop}}
}
\keyword{internal}

