% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tdmBigLoop.r
\name{tdmBigLoop}
\alias{tdmBigLoop}
\title{Tuning and unbiased evaluation in a big loop.}
\usage{
tdmBigLoop(envT, dataObj = NULL)
}
\arguments{
\item{envT}{an environment containing on input at least the element \code{tdm} (a list with general settings for TDMR, 
              see \code{\link{tdmDefaultsFill}}), which has at least the elements  
\describe{
\item{\code{tdm$runList}}{ vector of configuration names \code{.conf} }
}}

\item{dataObj}{\code{[NULL]} optional object of class \code{\link{TDMdata}} (the same for all runs in big loop). 
If it is \code{NULL}, it will be constructed here with the help of \code{\link{tdmReadAndSplit}}.
Then it can be different for each configuration object in the big loop.}
}
\value{
environment \code{envT}, containing  the results
     \item{res}{ data frame with results from last tuning (one line for each call of \code{tdmStart*})} 
     \item{bst}{ data frame with the best-so-far results from last tuning (one line collected after each (SPO) step)}
     \item{resGrid}{  list with data frames \code{res} from all tuning runs. Use \cr
           \code{envT$getRes(envT,confFile,nExp,theTuner)}  \cr
       to retrieve a specific \code{res}. }
     \item{bstGrid}{  list with data frames \code{bst} from all tuning runs. Use \cr
           \code{envT$getBst(envT,confFile,nExp,theTuner)}  \cr
       to retrieve a specific \code{bst}. }
     \item{theFinals}{ data frame with one line for each triple \code{(confFile,nExp,tuner)}, each line contains summary
       information about the tuning run in the form: \cr
           \code{confFile tuner nExp [params] NRUN NEVAL RGain.bst RGain.* sdR.*} \cr
       where \code{[params]} is written depending on \code{tdm$withParams}. \cr
       \code{NRUN} is the number of unbiased evaluation runs. \cr
       \code{NEVAL} is the number of function evaluations (model builds) during tuning. \cr
       \code{RGain} denotes the relative gain on a certain data set: the actual gain achieved with the model 
       divided by the maximum gain possible for the current cost matrix and the current data set. This is for classification
       tasks, in the case of regression each \code{RGain.*} is replaced by \code{RMAE.*}, the relative mean absolute error. \cr
       Each 'sdR.' denotes the standard deviation of the preceeding RGain or RMAE. \cr
       RGain.bst is the best result during tuning obtained on the training-validation data. RGain.avg is the average result 
       during tuning. The following pairs {RGain.* sdR.*} are the results of one or several unbiased evaluations on the test data
       where '*' takes as many values as there are elements in \code{tdm$umode} (the possible values are explained in 
       \code{\link{unbiasedRun}}).  
       }
     \item{result}{ object of class \code{\link{TDMclassifier}} or \code{\link{TDMregressor}}. This is a list with results from \code{tdm$mainFunc} 
         as called in the last unbiased evaluation using the best parameters found during tuning. 
         Use \code{\link[=print.TDMclassifier]{print}(envT$result)} to get more info on such an object of class \code{\link{TDMclassifier}}.  }
     \item{tunerVal}{ an object with the return value from the last tuning process. For every tuner, this is the list 
         \code{spotConfig}, containing the SPOT settings plus the TDMR settings in elements \code{opts} and \code{tdm}. Every tuner 
         extends this list by \code{tunerVal$alg.currentResult} and \code{tunerVal$alg.currentBest}, see \code{\link{tdmDispatchTuner}}.
         In addition, each tuning method might add specific elements to the list, see the description of each tuner. }
  Environment \code{envT} contains further elements, but they are only relevant for the internal operation of 
  \code{tdmBigLoop} and its subfunctions.
}
\description{
For each configuration object \code{.conf} in \code{tdm$runList} call all tuning algorithms (SPOT, CMA-ES 
or other) specified in \code{tdm$tuneMethod} (via function \code{\link{tdmDispatchTuner}}).
After each tuning process perform a run of \code{tdm$unbiasedFunc} 
(usually \code{\link{unbiasedRun}}). \cr
Each of these experiments is repeated \code{tdm$nExperim} times. Thus we have for each tripel \cr
   \tabular{ll}{
       \tab \code{   (confName,nExp,theTuner)} \cr
   }
a tuning result. The ranges of the triple elements are: \cr
   \tabular{ll}{
       \tab \code{   confName in tdm$runList} \cr
       \tab \code{   nExp in 1,...,tdm$nExperim} \cr
       \tab \code{   theTuner in tdm$tuneMethod} \cr
   }
}
\details{
\code{tdm} refers to \code{envT$tdm}.

The available tuning algorithms (tuners) are 
     \itemize{
     \item{\code{\link{spotTuner}}:  Call \code{\link[SPOT]{spot}}.   }
     \item{\code{\link{lhdTuner}}:  Perform a parameter tuning using a Latin hypercube design (LHD) 
           for obtaining best design points. LHD is performed by configuring SPOT 
           in such a way that all the budget is used for the initial design (usually LHD). }
     \item{\code{\link{cma_jTuner}}:  Perform a parameter tuning by CMA-ES, using the *Java* 
           implementation by Niko Hansen through the interface package \code{\link[rCMA]{rCMA-package}}.    }
     \item{\code{\link{cmaesTuner}}:  Perform a parameter tuning by CMA-ES, using the *R*-implementation 
           (package \code{\link[cmaes]{cma_es}} by Olaf Mersmann) 
           (deprecated, use \code{\link{cma_jTuner}} instead).  }
     \item{\code{\link{bfgsTuner}}:   Perform a parameter tuning by Broyden, Fletcher, Goldfarb and Shanno (BFGS) method.
           The L-BFGS-B version allowing box constraints is used.  }
     }
}
\note{
Side effects:
  A compressed version of \code{envT } is saved to file \code{tdm$filenameEnvT} 
  (default: \code{<runList[1]>.RData}) in directory \code{tdm$path}. If \code{tdm$path==NULL}
  use the current directory.\cr
  If \code{tdm$U.saveModel==TRUE}, then \code{envT$result$lastRes$lastModel} (the last 
  trained model) will be saved to \code{tdm$filenameEnvT}. 
  The default is \code{tdm$U.saveModel==TRUE}. If \code{tdm$U.saveModel==FALSE} 
  then smaller \code{.RData} files will result.


Example usages of function \code{tdmBigLoop} are shown in \cr
\tabular{ll}{
   \tab \code{   demo(demo03sonar)} \cr 
   \tab \code{   demo(demo03sonar_B)} \cr
   \tab \code{   demo(demo04cpu)} \cr
}
where the corresponding R-sources are in directory \code{demo}.
}
\examples{
#*# This demo shows a complete tuned data mining process (level 3 of TDMR) where 
#*# the data mining task is the classification task SONAR (from UCI repository, 
#*# http://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+\%28Sonar,+Mines+vs.+Rocks\%29).
#*# The data mining process is in main_sonar.r, which calls tdmClassifyLoop and tdmClassify
#*# with Random Forest as the prediction model. 
#*# The three parameter to be tuned are CUTOFF1, CLASSWT2 and XPERC, as specified 
#*# in controlSC() (control_sonar.r). The tuner used here is LHD.  
#*# Tuning runs are rather short, to make the example run quickly. 
#*# Do not expect good numeric results. 
#*# See demo/demo03sonar_B.r for a somewhat longer tuning run, with two tuners SPOT and LHD.

  ## path is the dir with data and main_*.r file:
  path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
  #path <- paste("../../inst", "demo02sonar",sep="/");

  ## control settings for TDMR
  tdm <- list( mainFunc="main_sonar"
             , runList = c("sonar_04.conf")
             , umode="CV"              # { "CV" | "RSUB" | "TST" | "SP_T" }
             , tuneMethod = c("lhd")
             , filenameEnvT="exBigLoop.RData"   # file to save environment envT 
             , nrun=1, nfold=2         # repeats and CV-folds for the unbiased runs
             , nExperim=1
             , optsVerbosity = 0       # the verbosity for the unbiased runs
             );
  source(paste(path,"main_sonar.r",sep="/"));    # main_sonar, readTrnSonar
\dontshow{
  #*# This demo is for "R CMD check" (runs in half the time) 
  source(paste(path,"control_sonar_check.r",sep="/")); # controlDM, controlSC
}
\donttest{
  #*# This demo is for example and help (more meaningful, a bit higher budget)
  source(paste(path,"control_sonar.r",sep="/")); # controlDM, controlSC
}
  
  
  ctrlSC <- controlSC();
  ctrlSC$opts <- controlDM();
  
  # construct envT from settings given in tdm & sCList
  envT <- tdmEnvTMakeNew(tdm,sCList=list(ctrlSC));
  dataObj <- tdmReadTaskData(envT,envT$tdm);
  envT <- tdmBigLoop(envT,dataObj=dataObj);     # start the big tuning loop 
\dontshow{
  #*# This demo is for "R CMD check" (to remove the output files) 
  unlink(paste(envT$tdm$path,"exBigLoop.RData",sep="/"));
}
}
\seealso{
\code{\link{tdmDispatchTuner}}, \code{\link{unbiasedRun}}
}
\author{
Wolfgang Konen (\email{wolfgang.konen@th-koeln.de}), THK, Patrick Koch
}
