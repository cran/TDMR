\name{cmaesTuner}
\alias{cmaesTuner}
\title{Perform CMA-ES tuning (R version).}
\usage{
  cmaesTuner(confFile, tdm, envT, dataObj)
}
\arguments{
  \item{confFile}{task configuration for tuning algorithm}

  \item{tdm}{the TDMR object}

  \item{envT}{the environment variable}

  \item{dataObj}{the \code{\link{TDMdata}} object
  containing the data set (train/vali part and test part)}
}
\value{
  result of CMA-ES algorithm
}
\description{
  Perform a parameter tuning by CMA-ES, using the
  *R*-implementation (package \code{cmaes} by Olaf
  Mersmann).
}
\keyword{internal}

