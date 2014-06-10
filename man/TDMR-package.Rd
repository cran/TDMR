\docType{package}
\name{TDMR-package}
\alias{TDMR}
\alias{TDMR-package}
\title{Tuned Data Mining in R}
\description{
  Tuned Data Mining in R
}
\details{
  \tabular{ll}{ Package: \tab TDMR\cr Type: \tab Package\cr
  Version: \tab 1.1\cr Date: \tab 10.06.2014\cr License:
  \tab GPL (>= 2)\cr LazyLoad: \tab yes\cr }

  TDMR is a package for tuned data mining (predictive
  analytics, i.e. \bold{classification} and
  \bold{regression}). Its main features are: \cr 1) A
  variety of tuners, with special emphasis on \link{SPOT}
  (a well-known R package for parameter tuning), but also
  CMA-ES (package \code{\link[rCMA]{rCMA}}) and other
  tuning algorithms. \cr 2) Tuning of preprocessing
  (feature generation) parameters and model building
  parameters simultaneously.  \cr 3) Support for multiple
  tuning experiments (different settings, repetitions with
  different resamplings, ...).  \cr 4) Easy parallelization
  of those experiments with the help of R packages
  \code{snow} and \code{\link{parallel}}.  \cr 5)
  Extensibility: New tuning parameters, new feature
  preprocessing tools, model builders and even new tuners
  can be added easily.

  The main entry point functions are
  \code{\link{tdmClassifyLoop}},
  \code{\link{tdmRegressLoop}} and
  \code{\link{tdmBigLoop}}. See
  \code{\link{tdmOptsDefaultsSet}} and
  \code{\link{tdmDefaultsFill}} for an overview of
  adjustable TDMR-parameters.
}
\author{
  Wolfgang Konen (\email{wolfgang.konen@fh-koeln.de}),
  Patrick Koch
}
\references{
  \url{http://gociop.de/research-projects/tuned-data-mining/}
}
\keyword{data}
\keyword{learning}
\keyword{machine}
\keyword{mining}
\keyword{package}
\keyword{tuning}

