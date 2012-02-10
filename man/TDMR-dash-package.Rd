\name{TDMR-package}
\alias{TDMR-package}
\alias{TDMR-package}
\title{Tuned Data Mining in R}
\description{Tuned Data Mining in R}
\details{\tabular{ll}{
Package: \tab TDMR\cr
Type: \tab Package\cr
Version: \tab 0.2.0\cr
Date: \tab 10.02.2012\cr
License: \tab GPL (>= 3)\cr
LazyLoad: \tab yes\cr
}

TDMR is a package for tuned data mining (predictive analytics, i.e. \bold{classification} and \bold{regression}). Its main features are: \cr
1) A variety of tuners, with special emphasis on \link{SPOT} (another well-known R package for parameter tuning), but also CMA-ES and other tuning algorithms. \cr
2) Tuning of preprocessing (feature generation) model building parameters simultaneously.  \cr
3) Support for multiple tuning experiments (different settings, repetitions with different resamplings, ...).  \cr
4) Easy parallelization of those experiments with the help of R package \link{snowfall}.

The main entry point functions are \code{\link{tdmClassifyLoop}}, \code{\link{tdmRegressLoop}} and \code{\link{tdmCompleteEval}}. 
See \code{\link{tdmOptsDefaultsSet}} and \code{\link{tdmDefaultsFill}} for an overview of adjustable TDMR-parameters.}
\alias{TDMR}
\docType{package}
\author{Wolfgang Konen, Patrick Koch}
\references{\url{http://gociop.de/research-projects/tuned-data-mining/}}
\keyword{package}
\keyword{tuning}
\keyword{data}
\keyword{mining}
\keyword{machine}
\keyword{learning}

