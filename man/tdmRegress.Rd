\name{tdmRegress}
\alias{tdmRegress}
\title{Core regression function of TDMR.}
\usage{
  tdmRegress(d_train, d_test, d_preproc,
    response.variables, input.variables, opts)
}
\arguments{
  \item{d_train}{training set}

  \item{d_test}{test set, same columns as training set}

  \item{d_preproc}{data used for preprocessing. May be
  NULL, if no preprocessing is done (opts$PRE.SFA=="none"
  and opts$PRE.PCA=="none"). If preprocessing is done, then
  d_preproc is usually all non-validation data.}

  \item{response.variables}{name of column which carries
  the target variable - or - vector of names specifying
  multiple target columns (these columns are not used
  during prediction, only for evaluation)}

  \item{input.variables}{vector with names of input
  columns}

  \item{opts}{additional parameters [defaults in brackets]
  \describe{ \item{\code{SRF.*}}{ several parameters for
  sorted_rf_importance (see tdmModelingUtils.r) }
  \item{\code{RF.*}}{ several parameters for RF (Random
  Forest, defaults are set, if omitted) }
  \item{\code{SVM.*}}{ several parameters for SVM (Support
  Vector Machines, defaults are set, if omitted)}
  \item{\code{filename}}{ } \item{\code{data.title}}{ }
  \item{\code{MOD.method}}{ ["RF"] the main training method
  ["RF"|"SVM"|"LM"]: use [Random forest| SVM| linear model]
  for the main model} \item{\code{MOD.SEED}}{ =NULL: set
  the RNG to system time as seed (different RF trainings)
  =any value: set the random number seed to this value (+i)
  to get reproducible random numbers. In this way, the
  model training part (RF, NNET, ...) gets always a fixed
  seed.  (see also TST.SEED in tdmRegressLoop) }
  \item{\code{OUTTRAFO}}{ [NULL] string, apply a
  transformation to the output variable}
  \item{\code{fct.postproc}}{ [NULL] name of a user-def'd
  function for postprocessing of predicted output }
  \item{\code{gr.log}}{ =FALSE (def): make scatter plot
  as-is, =TRUE: transform output x with log(x+1) (x should
  be nonnegative) } \item{\code{GD.DEVICE}}{ if !="non",
  then make a pairs-plot of the 5 most important variables
  and make a true-false bar plot } \item{\code{VERBOSE}}{
  [2] =2: most printed output, =1: less, =0: no output } }}
}
\value{
  \code{res}, an object of class \code{tdmRegre}, this is a
  list containing \item{\code{d_train}}{ training set +
  predicted class column(s) } \item{\code{d_test}}{ test
  set + predicted target output } \item{\code{allRMAE}}{
  data frame with columns = (rmae.train, rmae.test,
  theil.train, theil.test, ...)  and rows = response
  variables. Here Theil's U is based on RMAE (relative mean
  absolute errror).  } \item{\code{allRMSE}}{ data frame
  with columns = (rmse.train, rmse.test, theil.train,
  theil.test, ...)  and rows = response variables. Here
  Theil's U is based on RMSE (root mean square error).  }
  \item{\code{lastModel}}{ the last model built (e.g. the
  last Random Forest in the case of MOD.method=="RF") }
  \item{\code{opts}}{ parameter list from input, some
  default values might have been added }

  The item \code{lastModel} is specific for the *last*
  model (the one built for the last response variable in
  the last run and last fold)
}
\description{
  tdmRegress is called by \code{\link{tdmRegressLoop}} and
  returns an object of class \code{tdmRegre}. \cr It trains
  a model on training set \code{d_train} and evaluates it
  on test set \code{d_test}.  If this function is used for
  tuning, the test set \code{d_test} plays the role of a
  validation set.
}
\examples{
#*# This example shows a simple data mining process (phase 1 of TDMR) for regression on
#*# dataset iris.
#*# The data mining process in tdmRegress calls randomForest as the prediction model.
#*# It is called  for 2 response variables. Therefore, the data frames allRMAE and allRMSE
#*# have 2 rows.
#*#
opts=tdmOptsDefaultsSet()                       # set all defaults for data mining process
gdObj <- tdmGraAndLogInitialize(opts);          # init graphics and log file

data(iris)
response.variables=c("Petal.Length","Petal.Width")                # names, not data (!)
input.variables=setdiff(names(iris),response.variables)
opts$rgain.type="rmae"
opts$NRUN=1

idx_train = sample(nrow(iris))[1:110]
d_train=iris[idx_train,]
d_vali=iris[-idx_train,]
res <- tdmRegress(d_train,d_vali,NULL,response.variables,input.variables,opts)

print(res$allRMAE)
print(res$allRMSE)
}
\author{
  Wolfgang Konen, FHK, Sep'2009 - Jun'2012
}
\seealso{
  \code{\link{print.tdmRegre}} \code{\link{tdmRegressLoop}}
  \code{\link{tdmClassifyLoop}}
}

