\name{tdmClassifyLoop}
\alias{tdmClassifyLoop}
\title{Core classification double loop of TDMR.}
\usage{tdmClassifyLoop(dset, response.variables, input.variables, opts)
}
\description{Core classification double loop of TDMR. It contains a doublee loop (opts$NRUN and CV-folds)
and calls \code{\link{tdmClassify}}. It is called  by all R-functions main_*.}
\value{\code{result},  an object of class TDMclassifier, this is a list with results, containing
\item{opts}{ the res$opts from \code{\link{tdmClassify}}}
\item{lastRes}{ last run, last fold: result from \code{\link{tdmClassify}}}
\item{C_train}{ classification error on training set}
\item{G_train}{ gain on training set}
\item{R_train}{ relative gain on training set (percentage of max. gain on this set)}
\item{*_test}{ --- similar, with test set instead of training set}
\item{*_test2}{ --- similar, with test2 set instead of training set}

Each performance measure \code{C_*, G_*, R_*} is a vector of length \code{opts$NRUN}. To be specific, \code{C_train[i]} is the
classification error on the training set from the \code{i}-th run. This error is \code{mean(res$allEVAL$cerr.trn)}, i.e. the
mean of the classification errors from all response variables when \code{res} is the return value of  \code{\link{tdmClassify}}.
In the case of cross validation, for each performance measure an additional averaging over all folds is done.}
\seealso{\code{\link{print.TDMclassifier}}, \code{\link{tdmClassify}}, \code{\link{tdmRegress}}, \code{\link{tdmRegressLoop}}}
\author{Wolfgang Konen, FHK, Sep'2010 - Oct'2011}
\arguments{\item{dset}{the data frame for which cvi is needed}
\item{response.variables}{name of column which carries the target variable - or -
vector of names specifying multiple target columns
(these columns are not used during prediction, only for evaluation)}
\item{input.variables}{vector with names of input columns}
\item{opts}{a list from which we need here the following entries
\describe{
\item{\code{NRUN}}{ number of runs (outer loop)}
\item{\code{TST.SEED}}{ =NULL: get a new random number seed with \code{\link{tdmRandomSeed}}. =any value: set the random number seed
to this value to get reproducible random numbers and thus reproducible training-test-set-selection.
(only relevant in case TST.kind=="cv" or "rand") (see also MOD.SEED in \code{\link{tdmClassify}})}
\item{\code{TST.kind}}{ how to create cvi, handed over to \code{\link{tdmModCreateCVindex}}. If TST.kind="col", then cvi is taken from dset[,opts$TST.col].}
\item{\code{GD.RESTART}}{ [TRUE] =TRUE/FALSE: do/don't restart graphic devices}
\item{\code{GD.DEVICE}}{ ["non"|"win"|"pdf"|"png"]}
}}
}

