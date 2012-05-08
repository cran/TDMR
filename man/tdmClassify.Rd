\name{tdmClassify}
\alias{tdmClassify}
\title{Core classification function of TDMR.}
\usage{
  tdmClassify(d_train, d_test, d_dis, response.variables,
    input.variables, opts)
}
\arguments{
  \item{d_train}{training set}

  \item{d_test}{test set, same columns as training set}

  \item{d_dis}{'disregard set', i.e. everything what is
  neither train nor test. The model is applied to all
  records in d_dis (needed for active learning, see
  ssl_methods.r)}

  \item{response.variables}{name of column which carries
  the target variable - or - vector of names specifying
  multiple target columns (these columns are not used
  during prediction, only for evaluation)}

  \item{input.variables}{vector with names of input
  columns}

  \item{opts}{additional parameters [defaults in brackets]
  \describe{ \item{\code{SRF.*}}{ several parameters for
  \code{\link{tdmModSortedRFimport}} } \item{\code{RF.*}}{
  several parameters for RF (Random Forest, defaults are
  set, if omitted) } \item{\code{SVM.*}}{ several
  parameters for SVM (Support Vector Machines, defaults are
  set, if omitted)} \item{\code{filename}}{ }
  \item{\code{data.title}}{ } \item{\code{MOD.method}}{
  ["RF"] the main training method
  ["RF"|"MC.RF"|"SVM"|"NB"]: use [Random forest|
  MetaCost-RF| SVM| Naive Bayes] for the main model}
  \item{\code{MOD.SEED}}{ =NULL: get a new random number
  seed with \code{\link{tdmRandomSeed}} (different RF
  trainings). \cr =any value: set the random number seed to
  this value (+i) to get reproducible random numbers. In
  this way, the model training part (RF, NNET, ...) gets
  always a fixed seed (see also TST.SEED in
  \code{\link{tdmClassifyLoop}}) } \item{\code{CLASSWT}}{
  class weights (NULL, if all classes should have the same
  weight) (currently used only by methods RF, MC.RF and by
  \code{\link{tdmModSortedRFimport}}) }
  \item{\code{fct.postproc}}{ [NULL] name of user-def'd
  function for postprocessing of predicted output }
  \item{\code{GD.DEVICE}}{ if !="non", then make a
  pairs-plot of the 5 most important variables and make a
  true-false bar plot } \item{\code{VERBOSE}}{ [2] =2: most
  printed output, =1: less, =0: no output } }}
}
\value{
  \code{res}, an object of class \code{tdmClass}, this is a
  list containing \item{\code{d_train}}{ training set +
  predicted class column(s) } \item{\code{d_test}}{ test
  set + predicted class column(s) } \item{\code{d_dis}}{
  disregard set + predicted class column(s) }
  \item{\code{sumEVAL}}{ list with evaluation measures,
  summed over all response variables }
  \item{\code{allEVAL}}{ data frame with evaluation
  measures, one row for each response variable }
  \item{\code{lastCmTrain}}{ a list with evaluation info
  for training set (confusion matrix, gain, class errors,
  ...)  } \item{\code{lastCmTest}}{ a list with evaluation
  info for test set (confusion matrix, gain, class errors,
  ...) } \item{\code{lastModel}}{ the last model built
  (i.e. for the last response variable) }
  \item{\code{lastProbs}}{ a list with three probability
  matrices (row: records, col: classes) v_train, v_test,
  v_dis, if the model provides probabilities; NULL else. }
  \item{\code{lastPred}}{ name of the colum where the
  prediction of the last model is appended to the datasets
  d_train, d_test and d_dis } \item{\code{opts}}{ parameter
  list from input, some default values might have been
  added }

  The 9 evaluation measures in sumEVAL and allEVAL are
  cerr.* (misclassification errror), gain.* (total gain)
  and rgain.* (relative gain, i.e. total gain divided by
  max. achievable gain in *) where * = [trn | tst | tst2 ]
  stands for [ training set | test set | test set with
  special treatment ] and the special treatment is either
  opts$test2.string = "no postproc" or = "default cutoff".
  \cr\cr The five items \code{lastCmTrain},
  \code{lastCmTest}, \code{lastModel}, \code{lastProbs},
  \code{lastPred} are specific for the *last* model (the
  one built for the last response variable in the last run
  and last fold)
}
\description{
  tdmClassify is called by \code{\link{tdmClassifyLoop}}
  and returns an object of class \code{tdmClass}. \cr It
  trains a model on training set \code{d_train} and
  evaluates it on test set \code{d_test}.  If this function
  is used for tuning, the test set \code{d_test} plays the
  role of a validation set.
}
\details{
  Currently d_dis is allowed to be a 0-row data frame, but
  d_train and d_test must have at least one record. \cr
}
\author{
  Wolfgang Konen, FHK, Sep'2009 - Dec'2011
}
\seealso{
  \code{\link{print.tdmClass}}
  \code{\link{tdmClassifyLoop}}
  \code{\link{tdmRegressLoop}}
}

