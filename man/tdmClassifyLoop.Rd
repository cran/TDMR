\name{tdmClassifyLoop}
\alias{TDMclassifier}
\alias{tdmClassifyLoop}
\title{Core classification double loop of TDMR returning a \code{\link{TDMclassifier}} object.}
\usage{
  tdmClassifyLoop(dset, response.variables,
    input.variables, opts)
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
  (outer loop)} \item{\code{TST.SEED}}{ =NULL: get a new
  random number seed with \code{\link{tdmRandomSeed}}. =any
  value: set the random number seed to this value to get
  reproducible random numbers and thus reproducible
  training-test-set-selection.  (only relevant in case
  TST.kind=="cv" or "rand") (see also MOD.SEED in
  \code{\link{tdmClassify}})} \item{\code{TST.kind}}{ how
  to create cvi, handed over to
  \code{\link{tdmModCreateCVindex}}. If TST.kind="col",
  then cvi is taken from dset[,opts$TST.col].}
  \item{\code{GD.RESTART}}{ [TRUE] =TRUE/FALSE: do/don't
  restart graphic devices} \item{\code{GD.DEVICE}}{
  ["non"|"win"|"pdf"|"png"]} }}
}
\value{
  \code{result}, an object of class
  \code{\link{TDMclassifier}}, this is a list with results,
  containing \item{lastRes}{ last run, last fold: result
  from \code{\link{tdmClassify}}} \item{C_train}{
  classification error on training set} \item{G_train}{
  gain on training set} \item{R_train}{ relative gain on
  training set (percentage of max. gain on this set)}
  \item{*_test}{ --- similar, with test set instead of
  training set} \item{*_test2}{ --- similar, with test2 set
  instead of training set}

  Each performance measure \code{C_*, G_*, R_*} is a vector
  of length \code{opts$NRUN}. To be specific,
  \code{C_train[i]} is the classification error on the
  training set from the \code{i}-th run. This error is
  \code{mean(res$allEVAL$cerr.trn)}, i.e. the mean of the
  classification errors from all response variables when
  \code{res} is the return value of
  \code{\link{tdmClassify}}.  In the case of cross
  validation, for each performance measure an additional
  averaging over all folds is done.
}
\description{
  tdmClassifyLoop contains a double loop (opts$NRUN and
  CV-folds) and calls \code{\link{tdmClassify}}. It is
  called by all R-functions main_*. \cr It returns an
  object of class \code{\link{TDMclassifier}}.
}
\examples{
#*# --------- demo/demo00sonar.r ---------
#*# This demo shows a simple data mining process (phase 1 of TDMR) for the classification task
#*# SONAR (from UCI repository, http://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+\%28Sonar,+Mines+vs.+Rocks\%29).
#*# The data mining process is in main_sonar.r, which calls tdmClassifyLoop and tdmClassify.
#*# with Random Forest as the prediction model. 

## load package and set working directory
require("TDMR");
path <- paste(.find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");
oldwd <- getwd();
setwd(path);
source("main_sonar.r");

## set the elements of list opts. See ?tdmOptsDefaultsSet
## for a complete list of all default settings and explanatory comments
opts = tdmOptsDefaultsSet();    
opts$filename = "sonar.txt"
opts$READ.CMD = "read.csv2(file=paste(opts$dir.data, filename, sep=\\"\\"), dec=\\".\\", sep=\\",\\",header=FALSE)"
opts$data.title <- "Sonar Data";
      
result=main_sonar(opts);

setwd(oldwd);
}
\author{
  Wolfgang Konen (\email{wolfgang.konen@fh-koeln.de}), FHK,
  Sep'2010 - Apr'2012
}
\seealso{
  \code{\link{print.TDMclassifier}},
  \code{\link{tdmClassify}}, \code{\link{tdmRegress}},
  \code{\link{tdmRegressLoop}}
}

