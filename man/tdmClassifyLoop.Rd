\name{tdmClassifyLoop}
\alias{tdmClassifyLoop}
\title{Core classification double loop of TDMR.}
\usage{tdmClassifyLoop(dset, response.variables, input.variables, opts)
}
\description{Core classification double loop of TDMR. It contains a doublee loop (opts$NRUN and CV-folds)
and calls \code{\link{tdmClassify}}. It is called  by all R-functions main_*}
\details{INPUT}
\value{\code{result},  an object of class TDMclassifier, this is a list with results, containing
\item{opts}{ the res$opts from \code{\link{tdmClassify}}} 
\item{last_res}{ last run, last fold: result from \code{\link{tdmClassify}}} 
\item{C_train}{ classification error on training set} 
\item{G_train}{ gain on training set} 
\item{R_train}{ relative gain on training set (percentage of max. gain on this set)} 
\item{*_test}{ --- similar, with test set instead of training set} 
\item{*_test2}{ --- similar, with test2 set instead of training set}}
\references{\code{\link{tdmClassify}}, \code{\link{tdmRegress}}, \code{\link{tdmRegressLoop}}}
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

