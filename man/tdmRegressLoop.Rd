\name{tdmRegressLoop}
\alias{tdmRegressLoop}
\alias{TDMregressor}
\title{Core regression double loop of TDMR returning a \code{\link{TDMregressor}} object.}
\usage{
  tdmRegressLoop(dset, response.variables, input.variables,
    opts)
}
\arguments{
  \item{dset}{the data frame for which cvi is needed}

  \item{response.variables}{name of column which carries
  the target variable - or - vector of names specifying
  multiple target columns (these columns are not used
  during prediction, only for evaluation)}

  \item{input.variables}{vector with names of input
  columns}

  \item{opts}{a list from which we need here the following
  entries \describe{ \item{\code{NRUN}}{ number of runs
  (outer loop)} \item{\code{TST.SEED}}{ =NULL: leave the
  random number seed as it is. =any value: set the random
  number seed to this value to get reproducible random
  numbers and thus reproducible
  training-test-set-selection.  (only relevant in case
  TST.kind=="cv" or "rand") (see also MOD.SEED in
  \code{\link{tdmClassify}})} \item{\code{TST.kind}}{ how
  to create cvi, handed over to
  \code{\link{tdmModCreateCVindex}}. If TST.kind="col",
  then cvi is taken from dset[,opts$TST.col].}
  \item{\code{GD.RESTART}}{ [TRUE] =TRUE/FALSE: do/don't
  restart graphic devices} \item{\code{GRAPHDEV}}{ ["non"|
  other ]} }}
}
\value{
  \code{result}, an object of class
  \code{\link{TDMregressor}}, this is a list with results,
  containing \item{opts}{ the res$opts from
  \code{\link{tdmRegress}}} \item{lastRes}{ last run, last
  fold: result from \code{\link{tdmRegress}}}
  \item{R_train}{ RMAE / RMSE on training set (vector of
  length NRUN), depending on opts$rgain.type=="rmae" or
  "rmse"} \item{S_train}{ RMSE on training set (vector of
  length NRUN)} \item{T_train}{ Theil's U for RMAE on
  training set (vector of length NRUN)} \item{*_test}{ ---
  similar, with test set instead of training set --- }
  \item{Err}{ } \item{predictions}{ last run: data frame
  with dimensions [nrow(dset),length(response.variable)].
  In case of CV, all validation set predictions (for each
  record in dset), in other cases mixed validation / train
  set predictions.  }
}
\description{
  tdmRegressLoop contains a double loop (opts$NRUN and
  CV-folds) and calls \code{\link{tdmRegress}}. It is
  called by all R-functions main_*. \cr It returns an
  object of class \code{\link{TDMregressor}}.
}
\examples{
#*# --------- demo/demo01cpu.r ---------
#*# This demo shows a simple data mining process (phase 1 of TDMR) for the regression task
#*# CPU (from UCI repository, http://archive.ics.uci.edu/ml/datasets/Computer+Hardware).
#*# The data mining process is in main_cpu.r, which calls tdmRegressLoop and tdmRegress
#*# with Random Forest as the prediction model. 

## load package and set working directory
#library(TDMR);
path <- paste(find.package("TDMR"), "demo01cpu",sep="/");
#path <- paste("../inst", "demo01cpu",sep="/");
oldwd <- getwd();
setwd(path);
source("main_cpu.r");                # contains also readCmdCpu()

source("cpu_00.apd",local=TRUE);     # set opts 

result=main_cpu(opts);

## restore old working directory
setwd(oldwd);

}
\author{
  Wolfgang Konen (\email{wolfgang.konen@fh-koeln.de}), FHK,
  Sep'2010 - Jun'2012
}
\seealso{
  \code{\link{tdmRegress}}, \code{\link{tdmClassifyLoop}},
  \code{\link{tdmClassify}}
}

