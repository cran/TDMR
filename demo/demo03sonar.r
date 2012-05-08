#*# --------- demo/demo03sonar.r ---------
#*# This demo shows a complete tuned data mining process (phase 3 of TDMR) where the data mining task is the classification task
#*# SONAR (from UCI repository, http://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+%28Sonar,+Mines+vs.+Rocks%29).
#*# The data mining process is in main_sonar.r, which calls tdmClassifyLoop and tdmClassify
#*# with Random Forest as the prediction model. 
#*# The three parameter to be tuned are CUTOFF1, CLASSWT2 and XPERC, as specified in file sonar_04.roi.
#*# The tuners used here are SPOT and LHD.  
#*# Tuning runs are rather short, to make the example run quickly. Do not expect good numeric results. 
#*# See demo/demo03sonar_B.r for a somewhat longer tuning run.

## load package and set working directory (dir with .apd, .conf and main_*.r file)
library(TDMR);
path <- paste(.find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");
oldwd <- getwd();
setwd(path);
source("main_sonar.r");    # in working dir

## preliminary settings for TDMR
tdm <- list( mainFunction="main_sonar"
            , umode=c("RSUB")           # ["CV" | "RSUB" | "TST"]
            , tuneMethod = "spot"       # c("spot","lhd")
            , filenameEnvT="demo03.RData"   # file to save environment envT (in working dir)
            , finalFile="sonar.fin"
            , nrun=1, nfold=2         # repeats and CV-folds for the unbiased runs
            , withParams=TRUE         # list the columns with tuned parameter in final results 
            , nExperim=1
            , optsVerbosity = 0       # the verbosity for the unbiased runs
            );
## fill in other defaults for list tdm            
tdm <- tdmDefaultsFill(tdm);      
## sonar_04.conf has the settings for the tuning process (e.g. "auto.loop.steps"=number of SPOT generations       
## "auto.loop.evals"=budget of model building runs and io.roiFileName = "sonar_04.roi").
## runList could contain other files as well (e.g. c("sonar_01.conf","sonar_02.conf","sonar_03.conf")), if desired.
runList = c("sonar_04.conf");     
## spotStep can be either "auto" (do automatic tuning) or "rep" (make a visual report of best results)
spotStep = "auto";

## the call to tdmCompleteEval will start the whole TDMR process:
## - for each file in runList a complete DM tuning is started with each tuning method
## - the best result from tuning are fed into an unbiased model building and evaluation run 
## - results are printed and returned in envT$theFinals 
## - more detailed results are in other elements of envT
## - two plots: 
##      a) the progression of the response variable Y and the parameter variables during tuning
##      b) the sensitivity plot for each parameter in the vicinity of the best solution found 
envT <- tdmCompleteEval(runList,NULL,spotStep,tdm);

## restore old working directory
setwd(oldwd);

## the resulting tuning surface (the metamodel) can be inspected interactively with
##      load(paste(path,tdm$filenameEnvT,sep="/");     
##      tdmPlotResMeta(envT);
## (load(...) is only needed for reloading envT in another R-session)
