\name{tdmRegress}
\alias{tdmRegress}
\title{Core regression function of TDMR.}
\usage{tdmRegress(d_train, d_test, response.variables, input.variables, opts)
}
\description{Core regression function of TDMR. It is called by \code{\link{tdmRegressLoop}}.}
\value{a list \code{res} with results, containing
\item{\code{d_train}}{ training set + predicted class column(s) }
\item{\code{d_test}}{ test set + predicted target output }
\item{\code{rmse}}{ root mean square error (on test + train set) + Theil's U (on test + train set) }
\item{\code{rmae}}{ relative mean absolute error (on test + train set) + Theil's U  (on test + train set).
rmse and rmae are lists. If there is more than one response variable, then rmse and rmae 
contain the *sum* over response.variables for each list-entry. }
\item{\code{allRMAE}}{ data frame with columns = list-entries in rmae and rows = response variables  }
\item{\code{lastModel}}{       the last model built (e.g. the last Random Forest in the case of MOD.method=="RF") }
\item{\code{opts}}{ parameter list from input, some default values might have been added }

The item \code{lastModel} is 
specific for the *last* model (the one built for the last response variable in the last run and last fold)}
\author{Wolfgang Konen, FHK, Sep'2009 - Oct'2011}
\arguments{\item{d_train}{training set}
\item{d_test}{test set, same columns as training set}
\item{response.variables}{name of column which carries the target variable - or - 
vector of names specifying multiple target columns
(these columns are not used during prediction, only for evaluation)}
\item{input.variables}{vector with names of input columns}
\item{opts}{additional parameters [defaults in brackets]
\describe{
\item{\code{SRF.*}}{ several parameters for sorted_rf_importance (see tdmModelingUtils.r) }
\item{\code{RF.*}}{ several parameters for RF (Random Forest, defaults are set, if omitted)  }
\item{\code{SVM.*}}{ several parameters for SVM (Support Vector Machines, defaults are set, if omitted)}
\item{\code{filename}}{ }
\item{\code{data.title}}{ }
\item{\code{MOD.method}}{ ["RF"] the main training method
["RF"|"SVM"|"LM"]: use [Random forest|  SVM| linear model] for the main model}
\item{\code{MOD.SEED}}{ =NULL: set the RNG to system time as seed (different RF trainings)
=any value: set the random number seed to this value (+i) to get reproducible random
numbers. In this way, the model training part (RF, NNET, ...) gets always a fixed seed.
(see also TST.SEED in tdmRegressLoop) }
\item{\code{OUTTRAFO}}{ string, apply a transformation to the output variable}
\item{\code{fct.postproc}}{ user-def'd function for postprocessing of predicted output  }
\item{\code{gr.log}}{ =FALSE (def): make scatter plot as-is, 
=TRUE: transform output x with log(x+1) (x should be nonnegative) }
\item{\code{GRAPHDEV}}{ if !="non", then make a pairs-plot of the 5 most important variables
and make a true-false bar plot }
\item{\code{VERBOSE}}{ [2] =2: most printed output, =1: less, =0: no output }
}}
}

