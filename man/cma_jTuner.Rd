\name{cma_jTuner}
\alias{cma_jTuner}
\title{Perform CMA-ES tuning (Java version).}
\usage{
  cma_jTuner(confFile, tdm, envT, dataObj)
}
\arguments{
  \item{confFile}{task configuration for tuning algorithm}

  \item{tdm}{the list with TDMR settings. \cr If
  \code{tdm$CMA.propertyFile} is not NULL, then
  \code{\link{rCMA}} will read this CMA property file. If
  NULL, the default file CMAEvolutionStrategy.properties
  from find.package("rCMA") is taken. \cr If
  \code{tdm$CMA.populationSize} is not NULL, the CMA
  population size will be set accordingly. If NULL, the
  default population size \code{4 + 3*log(N)} is taken with
  \code{N} = number of parameters in .roi file.}

  \item{envT}{the environment variable}

  \item{dataObj}{the \code{\link{TDMdata}} object
  containing the data set (train/vali part and test part)}
}
\value{
  the result of CMA-ES tuning, i.e. the list
  \code{envT$spotConfig}, extended by list \code{cma} with
  elements \item{\code{cma$sres}}{ a string vector with all
  console output from the optimization run
  \code{cmaOptimDP} of package \code{\link{rCMA}} }
  \item{\code{cma$count}}{ the number of calls to
  \code{tdmStartOther} }
}
\description{
  Perform a parameter tuning by CMA-ES, using the *Java*
  implementation by Niko Hansen through the interface
  package \code{\link{rCMA}}.
}
\keyword{internal}

