#*# --------- demo/demo04cpu.r ---------
#*# This demo shows a complete tuned data mining process (level 3 of TDMR) where the 
#*# data mining task is the regression task CPU (from UCI repository,
#*# http://archive.ics.uci.edu/ml/datasets/Computer+Hardware).
#*# The data mining process is in main_cpu.r, which calls tdmRegressLoop and tdmRegress
#*# with Random Forest as the prediction model. 
#*# The two parameter to be tuned are MTRY and XPERC, as specified in file cpu_01.roi.
#*# The tuner used here is SPOT (the default in tdmDefaultsFill).

path <- paste(find.package("TDMR"), "demo01cpu/",sep="/");
#path <- paste("../inst", "demo01cpu/",sep="/");

## activate these two lines to source all R-files from TDMR/R:
#tdm$tdmPath <- ".."
#source("../inst/start.tdm.r",local=T); 

## preliminary settings for TDMR
tdm <- list(    mainFunc="main_cpu"
              , path="."
              , runList="cpu_01.conf"
              , umode="SP_T"            # ["CV" | "RSUB" | "TST" | "SP_T" ]
              , tuneMethod=c("spot","lhd")
              , filenameEnvT="demo04cpu.RData"   # file to save environment envT (in working dir)
              , finalFile="cpu.fin"     # where to write final results (best solution & unbiased eval for each tuner/.conf-combination)
              , withParams=TRUE         # list the columns with tuned parameter in final results 
              , optsVerbosity=1         # the verbosity for the unbiased run
              , nExperim=2
              , nrun=2
              );
## Each element of tdm$runList has the settings for one tuning process (e.g. 
##    - auto.loop.steps = number of SPOT generations       
##    - auto.loop.evals = budget of model building runs and 
##    - io.roiFileName = "cpu_01.roi"
## ). 

tdm <- tdmDefaultsFill(tdm);            # fill in defaults for tdm$TST.testFrac and others

spotStep = "auto";    ## spotStep can be either "auto" (do automatic tuning) or 
                      ## "rep" (make a visual report and an unbiased run on best results)
source(paste(path,"main_cpu.r",sep="/"));    
source(paste(path,"start_bigLoop.r",sep="/"),chdir=TRUE);    # change dir to 'path' while sourcing

## the resulting tuning surface (the metamodel) can be inspected interactively with
##      envT <- tdmEnvTLoad(paste(path,tdm$filenameEnvT,sep="/"));     
##      tdmPlotResMeta(envT);
## (tdmEnvTLoad(...) is only needed for reloading envT in another R-session) 
