\name{tdmCompleteEval}
\alias{tdmCompleteEval}
\title{Tuning and unbiased evaluation in a big loop.}
\usage{
  tdmCompleteEval(runList, spotList = NULL,
    spotStep = "auto", tdm)
}
\arguments{
  \item{runList}{vector of \code{.conf} filenames}

  \item{spotList}{\code{[NULL]} vector of \code{.conf}
  filenames for which spot tuning is done.  If \code{NULL},
  then \code{spotList=runList}.}

  \item{spotStep}{\code{["auto"]} which step of SPOT to
  execute (either \code{"auto"} or \code{"rep"}). Entries
  in this vector are cyclically recycled if \code{spotStep}
  is shorter than \code{runList}.}

  \item{tdm}{a list from which we need here the elements
  \describe{ \item{\code{mainFunc}}{ name of the
  DM-function to be called for tuning and for unbiased
  evaluations.  See \code{\link{tdmDefaultsFill}} how a
  default is found, if \code{mainFunc} is missing}
  \item{\code{mainFile}}{ if not NULL, source this file and
  change to the directory of mainFile before starting
  mainFunc} \item{\code{tuneMethod}}{vector of tuning
  method(s) \code{[ "spot", "cmaes", "lhd", "bfgs",
  "powell" ]} , see \code{\link{spotTuner}},
  \code{\link{cmaesTuner}}, \code{\link{lhdTuner}},
  \code{\link{bfgsTuner}}, \code{\link{powellTuner}} }
  \item{\code{unbiasedFunc}}{name of the function for
  unbiased evaluations to call} \item{\code{umode}}{a
  vector of strings containing the unbiased resampling
  strategies to execute \code{"RSUB", "TST", "CV", "SP_T"},
  see \code{\link{unbiasedRun}} }
  \item{\code{fileMode}}{[TRUE]: see "Note" section}
  \item{\code{finalFile}}{filename where to save
  \code{envT$theFinals}, only relevant for
  \code{tdm$fileMode==TRUE}}
  \item{\code{experFile}}{filename where to append
  \code{envT$theFinals}, only relevant for
  \code{tdm$fileMode==TRUE}}
  \item{\code{withParams}}{[length(runList)==1] Boolean, if
  \code{=TRUE}: include best parameters as columns in
  output \code{envT$theFinals}.  If \code{=FALSE}: don't
  (this is appropriate if \code{runList} combines several
  .conf files which differ in their parameters)}
  \item{\code{timeMode}}{ [1] see
  \code{\link{tdmDefaultsFill}} }
  \item{\code{SPLIT.SEED}}{[NULL] see
  \code{\link{tdmSplitTestData}} }
  \item{\code{TST.trnFrac}}{[NULL] if not NULL, copy it to
  opts$TST.trnFrac }
  \item{\code{filenameEnvT}}{[\code{<runList[1]>.RData}]
  the \code{.RData} file where environment \code{envT} is
  saved.  } }}
}
\value{
  environment \code{envT}, containing the results
  \item{res}{ data frame with results from last tuning (one
  line for each call of \code{tdmStart*})} \item{bst}{ data
  frame with the best-so-far results from last tuning (one
  line collected after each (SPO) step)} \item{resGrid}{
  list with data frames \code{res} from all tuning runs.
  Use \cr \code{envT$getRes(confFile,nExp,theTuner)} \cr to
  retrieve a specific \code{res}. } \item{bstGrid}{ list
  with data frames \code{bst} from all tuning runs. Use \cr
  \code{envT$getBst(confFile,nExp,theTuner)} \cr to
  retrieve a specific \code{bst}. } \item{theFinals}{ data
  frame with one line for each triple
  \code{(confFile,nExp,tuner)}, each line contains summary
  information about the tuning run in the form: \cr
  \code{confFile tuner nExp [params] NRUN NEVAL RGain.bst
  RGain.* sdR.*} \cr where \code{[params]} is written
  depending on \code{tdm$withParams}. \cr \code{NRUN} is
  the number of unbiased evaluation runs. \cr \code{NEVAL}
  is the number of function evaluations (model builds)
  during tuning. \cr \code{RGain} denotes the relative gain
  on a certain data set: the actual gain achieved with the
  model divided by the maximum gain possible for the
  current cost matrix and the current data set. This is for
  classification tasks, in the case of regression each
  \code{RGain.*} is replaced by \code{RMAE.*}, the relative
  mean absolute error. \cr Each 'sdR.' denotes the standard
  deviation of the preceeding RGain or RMAE. \cr RGain.bst
  is the best result during tuning obtained on the
  training-validation data. RGain.avg is the average result
  during tuning. The following pairs {RGain.* sdR.*} are
  the results of one or several unbiased evaluations on the
  test data where '*' takes as many values as there are
  elements in \code{tdm$umode} (the possible values are
  explained in \code{\link{unbiasedRun}}).  }
  \item{result}{ object of class TDMclassifier or
  TDMregressor. This is a list with results from
  \code{tdm$mainFunc} as called in the last unbiased
  evaluation using the best parameters found during tuning.
  Use
  \code{\link[=print.TDMclassifier]{print}(envT$result)} to
  get more info on such an object of class TDMclassifier.
  \cr See \code{\link{tdmClassifyLoop}} or
  \code{\link{tdmRegressLoop}} for further info on
  TDMclassifier or TDMregressor, resp. } \item{tunerVal}{
  an object with the return value from the last tuning
  process. For every tuner, this is the list
  \code{spotConfig}, containing the SPOT settings plus the
  TDMR settings in elements \code{opts} and \code{tdm}.
  Every tuner estends this list by
  \code{tunerVal$alg.currentResult} and
  \code{tunerVal$alg.currentBest}, see
  \code{\link{tdmDispatchTuner}}.  In addition, each tuning
  method might add specific elements to the list, see the
  description of each tuner. } Environment \code{envT}
  contains further elements, but they are only relevant for
  the internal operation of \code{tdmCompleteEval} and its
  subfunctions.
}
\description{
  For each \code{.conf} file in \code{runList} call all
  tuning algorithms (SPOT, CMA-ES or other) specified in
  \code{tdm$tuneMethod} (via function
  \code{\link{tdmDispatchTuner}}). For each tuning process
  perform one or several runs of \code{tdm$unbiasedFunc}
  (as many as \code{tdm$umode} has elements). Usually,
  \code{tdm$unbiasedFunc = \link{unbiasedRun}}.
}
\details{
  Tuning is skipped if the \code{.conf} file does not
  appear in \code{spotList} or if \code{spotStep!="auto"}.
  In this case it is assumed then that the appropriate
  \code{.bst} and \code{.res} files exist already.

  The available tuning algorithms (tuners) are \itemize{
  \item{\code{\link{spotTuner}}: Call \code{\link{spot}}.
  } \item{\code{\link{lhdTuner}}: Perform a parameter
  tuning using a Latin hypercube design (LHD) for obtaining
  best design points. LHD is performed by configuring SPOT
  in such a way that all the budget is used for the initial
  design (usually LHD). } \item{\code{\link{cmaesTuner}}:
  Perform a parameter tuning by CMA-ES, using the
  *R*-implementation (package \code{cmaes} by Olaf
  Mersmann).  } \item{\code{\link{cma_jTuner}}: Perform a
  parameter tuning by CMA-ES, using the *Java*
  implementation by Niko Hansen.  }
  \item{\code{\link{bfgsTuner}}: Perform a parameter tuning
  by Broyden, Fletcher, Goldfarb and Shanno (BFGS) method.
  The L-BFGS-B version allowing box constraints is used.  }
  \item{\code{\link{powellTuner}}: Perform a parameter
  tuning by Powell's UObyQA algorithm (unconstrained
  optimization by quadratic approximation), see R-package
  \code{powell}.  } }
}
\note{
  Side effects: Irrespective of the value of
  \code{tdm$fileMode}, \itemize{ \item a compressed version
  of \code{envT } is saved to file \code{tdm$filenameEnvT}
  (default: \code{<runList[1]>.RData}), relative to the
  directory of the \code{.conf} file.  }

  If \code{tdm$fileMode==TRUE}, more files are written
  relative to the directory of the \code{.conf} file:
  \itemize{ \item \code{envT$theFinals } is written to file
  \code{tdm$finalFile} and appended to \code{tdm$experFile}
  \item \code{envT$res } is written to a \code{.res} file
  in directory \code{<tuneMethod>} \item \code{envT$bst }
  is written to a \code{.bst} file in directory
  \code{<tuneMethod>} } More precisely: If
  \code{tdm$fileMode==TRUE} and we tune with
  \code{tuneMethod="lhd"} while performing the 3rd
  experiment for \code{.conf} file \code{cpu_01.conf}, then
  the \code{.res} file \code{lhd/cpu_01_lhd_03.res} is
  written relative to the directory of \code{.conf} file.
  Analoguously for \code{.bst} file.  \cr If
  \code{tdm$finalFile==NULL}, then it is set to
  \code{sub(".conf",".fin",runList[1]}.  \cr If
  \code{tdm$experFile==NULL}, then nothing is appended to
  \code{tdm$experFile}.

  Example usages of function \code{tdmCompleteEval} are
  shown with \cr \code{ demo(demo03sonar)},\cr \code{
  demo(demo03sonar_B)} and \cr \code{ demo(demo04cpu)} \cr
  where the corresponding R-sources are in directory
  \code{demo}.
}
\examples{
#*# --------- demo/demo03sonar.r ---------
#*# This demo shows a complete tuned data mining process (level 3 of TDMR) where 
#*# the data mining task is the classification task SONAR (from UCI repository, 
#*# http://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+\%28Sonar,+Mines+vs.+Rocks\%29).
#*# The data mining process is in main_sonar.r, which calls tdmClassifyLoop and tdmClassify
#*# with Random Forest as the prediction model. 
#*# The three parameter to be tuned are CUTOFF1, CLASSWT2 and XPERC, as specified 
#*# in file sonar_04.roi. The tuner used here is LHD.  
#*# Tuning runs are rather short, to make the example run quickly. 
#*# Do not expect good numeric results. 
#*# See demo/demo03sonar_B.r for a somewhat longer tuning run, with two tuners SPOT and LHD.

## load package and set working directory (dir with .apd, .conf and main_*.r file)
#library(TDMR);
path <- paste(.find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");
oldwd <- getwd();  setwd(path);
source("main_sonar.r");    # from working dir

## preliminary settings for TDMR
tdm <- list( mainFunc="main_sonar"
            , runList = "sonar_04.conf"
            , umode=c("CV")           # ["CV" | "RSUB" | "TST" | "SP_T" ]
            , tuneMethod = c("lhd")   # c("spot","lhd")
            , filenameEnvT="demo03.RData"   # file to save environment envT (in working dir)
            , nrun=1, nfold=2         # repeats and CV-folds for the unbiased runs
            , nExperim=1
            , parallelCPUs=1
            , optsVerbosity = 0       # the verbosity for the unbiased runs
            );
## tdm$runList="sonar_04.conf" has the settings for the tuning process (e.g. 
##    - "auto.loop.steps"=number of SPOT generations       
##    - "auto.loop.evals"=budget of model building runs and 
##    - io.roiFileName = "sonar_04.roi"
## ). tdm$runList could contain other files as well (e.g. 
##    c("sonar_01.conf","sonar_02.conf","sonar_03.conf")
## ), if desired.

spotStep = "auto";    ## spotStep can be either "auto" (do automatic tuning) or 
            ## "rep" (make a visual report and an unbiased run on best results)

## construct an initial environment envT from the given TDMR settings in tdm
## (this contains also the fill-in of other defaults for tdm via
##      envT$tdm <- tdmDefaultsFill(tdm);
## )
envT <- tdmEnvTMakeNew(tdm);

## the call to tdmBigLoop will start the whole TDMR process:
## - for each file in tdm$runList a complete DM tuning is started with each tuning 
##   method tdm$tuneMethod  (if spotStep=="auto")
## - the best result from tuning is fed into an unbiased model build and evaluation run 
## - results are printed and returned in envT$theFinals 
## - more detailed results are in other elements of environment envT
## - two plots: 
##      a) the progression of the response variable Y and the parameter variables during tuning
##      b) the sensitivity plot for each parameter in the vicinity of the best solution found 
envT <- tdmBigLoop(envT,spotStep);

setwd(oldwd);               # restore old working directory

## the resulting tuning surface(s) (the metamodel(s)) can be inspected interactively with
##      load(paste(path,tdm$filenameEnvT,sep="/");     
##      tdmPlotResMeta(envT);
## (load(...) is only needed for reloading envT in another R-session)
}
\author{
  Wolfgang Konen (\email{wolfgang.konen@fh-koeln.de}),
  Patrick Koch
}
\seealso{
  \code{\link{tdmDispatchTuner}}, \code{\link{unbiasedRun}}
}

