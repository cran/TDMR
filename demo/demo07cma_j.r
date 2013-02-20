#*# --------- demo/demo07cma_j.r ---------
#*# This demo shows for tuner cma_j (CMA-ES, Java version) a complete tuned data mining process (TDMR, level 3). 
#*# Other settings are the same as in demo03sonar.r, except that we use sonar_03.conf as configuration file.
#*# 
#*# Note that tuner cma_j operates silently for about 1-2 minutes. During this time, log files 
#*#     cma_j.log, cma_j.err,  outcmaesdisp.dat
#*# will be written to <TDMR>/javabin where <TDMR> = .find.package("TDMR").

## load package and set working directory (dir with .apd, .conf and main_*.r file)
#library(TDMR);
#path <- paste(.find.package("TDMR"), "demo02sonar",sep="/");
path <- paste("../inst", "demo02sonar",sep="/");
oldwd <- getwd();
setwd(path);
source("main_sonar.r");    # in working dir

## preliminary settings for TDMR
tdm <- list(  mainFunc="main_sonar"
            , runList = "sonar_03.conf"
            , umode=c("RSUB")           # ["CV" | "RSUB" | "TST" | "SP_T" ]
            , tuneMethod = c("cma_j","spot")
            , filenameEnvT="demoSonar.RData"   # file to save environment envT (in working dir)
            , fileMode=FALSE
            , finalFile="sonar.fin"
            , nrun=5, nfold=2         # repeats and CV-folds for the unbiased runs
            , nExperim=1 #2
            , optsVerbosity = 0       # the verbosity for the unbiased runs
            );
## tdm$runList="sonar_03.conf" has the settings for the tuning process (e.g. 
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
envT <- tdmBigLoop(envT,spotStep);
# deprecated: envT <- tdmCompleteEval(runList,NULL,spotStep,tdm);

setwd(oldwd);         ## restore old working directory

## the resulting tuning surface (the metamodel) can be inspected interactively with
##      load(paste(path,tdm$filenameEnvT,sep="/");     
##      tdmPlotResMeta(envT);
## (load(...) is only needed for reloading envT in another R-session)
