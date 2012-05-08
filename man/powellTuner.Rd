\name{powellTuner}
\alias{powellTuner}
\title{Perform Powell's tuning.}
\usage{
  powellTuner(confFile, tdm, envT, dataObj)
}
\arguments{
  \item{confFile}{task configuration for tuning algorithm}

  \item{tdm}{the TDMR object}

  \item{envT}{the environment variable}

  \item{dataObj}{the \code{\link{TDMdata}} object
  containing the data set (train/vali part and test part)}
}
\value{
  result of Powell's algorithm
}
\description{
  Perform a parameter tuning by Powell's UObyQA algorithm
  (unconstrained optimization by quadratic approximation),
  see R-package \code{powell}
}
\keyword{internal}

