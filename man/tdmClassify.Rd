\name{tdmClassify}
\alias{tdmClassify}
\title{Core classification function of TDMR.}
\usage{tdmClassify(d_train, d_test, d_dis, response.variables, input.variables, opts)
}
\description{Core classification function of TDMR. It is called by \code{\link{tdmClassifyLoop}}.}
\details{Currently  d_dis is allowed to be a 0-row data frame, but d_train and d_test must have at least one record.
TODO: make code also applicable if d_test is a 0-row data frame.}
\value{a list \code{res} with results, containing
\item{\code{d_train}}{ training set + predicted class column(s) }
\item{\code{d_test}}{ test set + predicted class column(s) }
\item{\code{d_dis}}{  disregard set + predicted class column(s)  }
\item{\code{sumEVAL}}{ list with evaluation measures, summed over all response variables }
\item{\code{allEVAL}}{ data frame with evaluation measures, one row for each response variable
The following three items are specific for the *last* model (the one built for the last response variable): }
\item{\code{last.cm.train}}{ a list with evaluation info for training set (confusion matrix, gain, class errors, ...)  }
\item{\code{last.cm.test}}{  a list with evaluation info for test set (confusion matrix, gain, class errors, ...) }
\item{\code{last.rf}}{       the last model built (e.g. the last Random Forest in the case of method=="RF") }
\item{\code{last.probs}}{    probability matrix (row: records, col: classes), if the model provides probabilities; NULL else. }
\item{\code{opts}}{ parameter list from input, some default values might have been added }

The 9 evaluation measures in sumEVAL and allEVAL are
cerr.X    misclassification errror
gain.X    total gain
rgain.X   relative gain (% of max. gain in the specific set)
where X = [trn | tst | tst2 ] which stands for [ training set | test set | test set with special treatment ]
and the special treatment is either opts$test2.string = "no postproc" or = "default cutoff"}
\author{Wolfgang Konen, FHK, Sep'2009 - Oct'2011}
\arguments{\item{d_train}{training set}
\item{d_test}{test set, same columns as training set}
\item{d_dis}{'disregard set', i.e. everything what is neither train nor test. The model is 
applied to all records in d_dis (needed for active learning, see ssl_methods.r)}
\item{response.variables}{name of column which carries the target variable - or - 
vector of names specifying multiple target columns
(these columns are not used during prediction, only for evaluation)}
\item{input.variables}{vector with names of input columns}
\item{opts}{additional parameters [defaults in brackets]
\describe{
\item{\code{SRF.*}}{ several parameters for sorted_rf_importance (see tdmModelingUtils.r) }
\item{\code{RF.*}}{ several parameters for RF (Random Forest, defaults are set, if omitted)  }
\item{\code{SVM.*}}{ several parameters for SVM (Support Vector Machines, defaults are set, if omitted)}
\item{\code{method}}{ ["RF"] the main training method
["RF"|"MC.RF"|"SVM"|"NB"]: use [Random forest| MetaCost-RF| SVM| Naive Bayes] for the main model}
\item{\code{filename}}{ }
\item{\code{data.title}}{ }
\item{\code{MOD.SEED}}{ =NULL: set the RNG to system time as seed (different RF trainings)
=any value: set the random number seed to this value (+i) to get reproducible random
numbers. In this way, the model training part (RF, NNET, ...) gets always a fixed seed.
(see also TST.SEED in tdmClassifyLoop) }
\item{\code{CLASSWT}}{ class weights (NULL, if all classes should have the same weight)
(currently used only by methods RF, MC.RF and by SRF)  }
\item{\code{fct.postproc}}{ user-def'd function for postprocessing of predicted output  }
\item{\code{GRAPHDEV}}{ if !="non", then make a pairs-plot of the 5 most important variables
and make a true-false bar plot }
\item{\code{VERBOSE}}{ [2] =2: most printed output, =1: less, =0: no output }
}}
}

