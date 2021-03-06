% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tdmDispatchTuner.r
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

\item{dataObj}{the \code{\link{TDMdata}} object containing the data set (train/vali part and test part)}
}
\value{
the result of BFGS tuning, i.e. the list \code{envT$spotConfig}, extended by
   \item{\code{bfgs}}{ the return value from \code{optim(...,method="L-BFGS-B")}   }
   \item{\code{bfgs$count}}{ the number of calls to \code{tdmStartOther}  }
}
\description{
Perform a parameter tuning by Broyden, Fletcher, Goldfarb and Shanno (BFGS) method.
The L-BFGS-B version allowing box constraints is used.
}
\author{
Wolfgang Konen \email{wolfgang.konen@Th-koeln.de}, Patrick Koch
}
\keyword{internal}
