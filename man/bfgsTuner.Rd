\name{bfgsTuner}
\alias{bfgsTuner}
\title{Perform BFGS tuning.}
\usage{
  bfgsTuner(confFile, tdm, envT, dataObj)
}
\arguments{
  \item{confFile}{task configuration for tuning algorithm}

  \item{tdm}{the TDMR object}

  \item{envT}{the environment variable}

  \item{dataObj}{the \code{\link{TDMdata}} object
  containing the data set (train/vali part and test part)}
}
\value{
  result of BFGS algorithm
}
\description{
  Perform a parameter tuning by Broyden, Fletcher, Goldfarb
  and Shanno (BFGS) method. The L-BFGS-B version allowing
  box constraints is used.
}
\author{
  Wolfgang Konen, Patrick Koch
  \email{wolfgang.konen@fh-koeln.de}
}
\keyword{internal}

