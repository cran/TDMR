\name{cma_jTuner}
\alias{cma_jTuner}
\title{Perform CMA-ES tuning (Java version).}
\usage{
  cma_jTuner(confFile, tdm, envT, dataObj)
}
\arguments{
  \item{confFile}{task configuration for tuning algorithm}

  \item{tdm}{the TDMR object}

  \item{envT}{the environment variable}

  \item{dataObj}{the \code{\link{TDMdata}} object
  containing the data set (train/vali part and test part)}
}
\value{
  the result of CMA-ES tuning, i.e. the list
  \code{envT$spotConfig}, extended by
  \item{\code{cma$sres}}{ the return return string from the
  system call "java cma.examples.cmaJava CMAprops.txt" }
  \item{\code{cma$count}}{ the number of calls to
  \code{tdmStartOther} }
}
\description{
  Perform a parameter tuning by CMA-ES, using the *Java*
  implementation by Niko Hansen.
}
\keyword{internal}

