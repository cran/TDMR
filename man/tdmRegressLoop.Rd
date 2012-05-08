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
  similar, with test set instead of training set}
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
library(TDMR);
path <- paste(.find.package("TDMR"), "demo01cpu",sep="/");
#path <- paste("../inst", "demo01cpu",sep="/");
oldwd <- getwd();
setwd(path);
source("main_cpu.r");   # in working dir 

## set the elements of list opts. See ?tdmOptsDefaultsSet
## for a complete list of all default settings and explanatory comments
opts = tdmOptsDefaultsSet();    
      opts$dir.output <- paste("Output/", sep="")
      opts$filename = "cpu.csv"
      opts$READ.CMD = "read.csv2(file=paste(dir.data, filename, sep=\\"\\"), dec=\\".\\", na.string=\\"-1\\",nrow=opts$READ.NROW)"
      opts$SRF.kind = "ndrop"
      opts$SRF.ndrop =  2     # 0..n: how many variables (those with lowest importance) to drop
      opts$OCUT = 600         # cut records with output > OCUT (may be strong outliers, dropping
                              # them makes rmse$test and rmse$OOB faster converge)
      opts$fct.postproc <- "cpu.postproc";

      opts$gr.log=TRUE        # if =T: log(x+1)-transform for graphics "true vs. predicted"   
      opts$GD.DEVICE="win"    # ="pdf": all graphics to one multi-page PDF
                              # ="win": all graphics to (several) windows (X11)
      opts$data.title <- "CPU Data";
      opts$VERBOSE=2;      
      
result=main_cpu(opts);

## restore old working directory
setwd(oldwd);

}
\author{
  Wolfgang Konen, FHK, Sep'2010 - Oct'2011
}
\seealso{
  \code{\link{tdmRegress}}, \code{\link{tdmClassifyLoop}},
  \code{\link{tdmClassify}}
}

