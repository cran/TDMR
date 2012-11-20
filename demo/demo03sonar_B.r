#*# --------- demo/demo03sonar_B.r ---------
#*# Same as demo/demo03sonar.r, but with parameters for multiple tuning experiments & longer tuning runs:
#*#    in demo03sonar_B.r:    tdm$nExperim=2; tdm$nrun=5;
#*#    and in sonar_05.conf:  auto.loop.nevals = 50; init.design.size = 10;
#*# and with two tuners SPOT and LHD in comparison.

## load package and set working directory (dir with .apd, .conf and main_*.r file)
#library(TDMR);
path <- paste(.find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");
oldwd <- getwd();
setwd(path);
source("main_sonar.r");    # in working dir

## preliminary settings for TDMR
tdm <- list( mainFunction="main_sonar"
            , umode=c("RSUB")           # ["CV" | "RSUB" | "TST"]
            , tuneMethod = c("spot","lhd")
            , filenameEnvT="demo03.RData"   # file to save environment envT (in working dir)
            , finalFile="sonar.fin"
            , nrun=5, nfold=2         # repeats and CV-folds for the unbiased runs
            , withParams=TRUE         # list the columns with tuned parameter in final results 
            , nExperim=2
            , parallelCPUs=2
            , optsVerbosity = 0       # the verbosity for the unbiased runs
            );
## fill in other defaults for list tdm            
tdm <- tdmDefaultsFill(tdm);      
## sonar_05.conf has the settings for the tuning process (e.g. "auto.loop.steps"=number of SPOT generations       
## "auto.loop.evals"=budget of model building runs and io.roiFileName = "sonar_04.roi").
## runList could contain other files as well (e.g. c("sonar_01.conf","sonar_02.conf","sonar_03.conf")), if desired.
tdm$runList = c("sonar_05.conf");     
## spotStep can be either "auto" (do automatic tuning) or "rep" (make a visual report of best results)
spotStep = "auto";

## construct an initial environment envT from the given TDMR settings in tdm
## (this contains also the fill-in of other defaults for tdm via
##      envT$tdm <- tdmDefaultsFill(tdm);
## )
envT <- tdmEnvTMakeNew(tdm);

## the call to tdmBigLoop will start the whole TDMR process:
## - for each file in tdm$runList a complete DM tuning is started with each tuning method tdm$tuneMethod  (if spotStep=="auto")
## - the best result from tuning is fed into an unbiased model build and evaluation run 
## - results are printed and returned in envT$theFinals 
## - more detailed results are in other elements of environment envT
## - two plots: 
##      a) the progression of the response variable Y and the parameter variables during tuning
##      b) the sensitivity plot for each parameter in the vicinity of the best solution found 
envT <- tdmBigLoop(envT,spotStep);

#---- deprecated ----
## the call to tdmCompleteEval will start the whole TDMR process:
## - for each file in runList a complete DM tuning is started with each tuning method
## - the best result from tuning are fed into an unbiased model building and evaluation run 
## - results are printed and returned in envT$theFinals 
## - more detailed results are in other elements of envT
## - two plots: 
##      a) the progression of the response variable Y and the parameter variables during tuning
##      b) the sensitivity plot for each parameter in the vicinity of the best solution found 
#envT <- tdmCompleteEval(runList,NULL,spotStep,tdm);
#---- deprecated ----

## restore old working directory
setwd(oldwd);

## the resulting tuning surface (the metamodel) can be inspected interactively with
##      load(paste(path,tdm$filenameEnvT,sep="/");     
##      tdmPlotResMeta(envT);
## (load(...) is only needed for reloading envT in another R-session)
