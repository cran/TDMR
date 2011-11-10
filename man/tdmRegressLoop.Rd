\name{tdmRegressLoop}
\alias{tdmRegressLoop}
\title{Core regression double loop of TDMR.}
\usage{tdmRegressLoop(dset, response.variables, input.variables, opts)
}
\description{Core regression double loop of TDMR. It contains a doublee loop (opts$NRUN and CV-folds)
and calls \code{\link{tdmRegress}}. It is called  by all R-functions main_*}
\details{INPUT}
\value{\code{result},  an object of class TDMregressor, this is a list with results, containing
\item{opts}{ the res$opts from \code{\link{tdmRegress}}} 
\item{last_res}{ last run, last fold: result from \code{\link{tdmRegress}}} 
\item{R_train}{ RMAE / RMSE on training set (vector of length NRUN), depending on opts$rgain.type=="rmae" or "rmse"} 
\item{S_train}{ RMSE on training set (vector of length NRUN)} 
\item{T_train}{ Theil's U for RMAE on training set (vector of length NRUN)} 
\item{*_test}{ --- similar, with test set instead of training set}}
\references{\code{\link{tdmRegress}}, \code{\link{tdmClassifyLoop}}, \code{\link{tdmClassify}}}
\author{Wolfgang Konen, FHK, Sep'2010 - Oct'2011}
\arguments{\item{dset}{the data frame for which cvi is needed}
\item{response.variables}{name of column which carries the target variable - or - 
vector of names specifying multiple target columns
(these columns are not used during prediction, only for evaluation)}
\item{input.variables}{vector with names of input columns}
\item{opts}{a list from which we need here the following entries
\describe{
\item{\code{NRUN}}{ number of runs (outer loop)}
\item{\code{TST.SEED}}{ =NULL: leave the random number seed as it is. =any value: set the random number seed 
to this value to get reproducible random numbers and thus reproducible training-test-set-selection.
(only relevant in case TST.kind=="cv" or "rand") (see also MOD.SEED in \code{\link{tdmClassify}})}
\item{\code{TST.kind}}{ how to create cvi, handed over to \code{\link{tdmModCreateCVindex}}. If TST.kind="col", then cvi is taken from dset[,opts$TST.col].}
\item{\code{GD.RESTART}}{ [TRUE] =TRUE/FALSE: do/don't restart graphic devices}
\item{\code{GRAPHDEV}}{ ["non"| other ]}
}}
}

