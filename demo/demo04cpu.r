#*# This demo shows a complete tuned data mining process (phase 3 of TDMR) where the data mining task is the regression task
#*# CPU (from UCI repository, http://archive.ics.uci.edu/ml/datasets/Computer+Hardware).
#*# The data mining process is in main_cpu.r, which calls tdmRegressLoop and tdmRegress
#*# with Random Forest as the prediction model. 
#*# The two parameter to be tuned are MTRY and XPERC, as specified in file cpu_01.roi.
#*# The tuner used here is SPOT (the default in \code{\link{tdmDefaultsFill}}.

## load package and set working directory
library(TDMR);
path <- paste(.find.package("TDMR"), "demo01cpu",sep="/");
#path <- paste("../inst", "demo01cpu",sep="/");
oldwd <- getwd();
setwd(path);
source("main_cpu.r");   # in working dir 

## preliminary settings for TDMR
tdm <- list( mainFunction="main_cpu"
              , umode=c("RSUB","CV")    # ["CV" | "RSUB" | "TST"]
              , filenameEnvT="demoCpu.RData"   # file to save environment envT (in working dir)
              , finalFile="cpu.fin"     # where to write final results (best solution & unbiased eval for each tuner/.conf-combination)
              , withParams=TRUE         # list the columns with tuned parameter in final results 
              , optsVerbosity=0         # the verbosity for the unbiased runs
              );
## fill in other defaults for list tdm            
tdm <- tdmDefaultsFill(tdm);            
runList = c("cpu_01.conf");     
## cpu_01.conf has the settings for the tuning process (e.g. "auto.loop.steps"=number of SPOT generations       
## "auto.loop.evals"=budget of model building runs and io.roiFileName = "cpu_01.roi").
              
   
## the call to tdmCompleteEval will start the whole TDMR process:
## - for each file in runList a complete DM tuning is started with each tuning method
## - the best result from tuning are fed into an unbiased model building and evaluation run 
## - results are printed and returned in envT$theFinals 
## - more detailed results are in other elements of envT
## - two plots: 
##      a) the progression of the response variable Y and the parameter variables during tuning
##      b) the sensitivity plot for each parameter in the vicinity of the best solution found 
envT <- tdmCompleteEval("cpu_01.conf",NULL,"auto",tdm);

## restore old working directory
setwd(oldwd);

## the resulting tuning surface (the metamodel) can be inspected interactively with
##      load(paste(path,tdm$filenameEnvT,sep="/");     
##      tdmPlotResMeta(envT);
## (load(...) is only needed for reloading envT in another R-session)
