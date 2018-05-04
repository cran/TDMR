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
              , path=path
              #, runList="cpu_01.conf"
              , umode="SP_T"            # ["CV" | "RSUB" | "TST" | "SP_T" ]
              , tuneMethod=c("spot")    # "spot","lhd"
              , filenameEnvT="demo04cpu.RData"   # file to save environment envT (in working dir)
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

source(paste(path,"main_cpu.r",sep="/"));   


controlDM <- function() {
  #
  # settings for the DM process (former cpu_01.apd file): 
  #
  opts = list(path = path,
              dir.data = "data/",
              filename = "cpu.csv",
              READ.TrnFn = readTrnCpu,    # defined in main_sonar.r
              READ.NROW=100,
              data.title = "Cpu Data",
              MOD.method="RF",    # ["RF"|"MC.RF"|"SVM"|"NB" ...]
              OCUT = 600,         # cut records with output > OCUT (may be strong outliers, 
                                  # dropping them makes rmse$test and rmse$OOB faster
                                  # converge)
              SRF.kind = "xperc",
              RF.ntree = 50,
              RF.samp = 1000,
              fct.postproc="cpu.postproc",
              rgain.type="rmae",  # ["rmae" (default) |"rmse" ]
              gr.log=TRUE,        # if =T: log(x+1)-transform for graphics "true vs. predicted"   
              NRUN =  1,          # how many runs with different train & test samples  - or - 
              # how many CV-runs, if TST.kind="cv"
              GD.DEVICE="non",    # ["pdf"|"win"|"non"]: all graphics to 
              # [one multi-page PDF | (several) windows (X11) | dev.null]
              GD.RESTART=F,
              VERBOSE = 1,
              SRF.verbose = 1,
              logFile=FALSE       # no logfile (needed for Sweave/.Rnw only)
  );
  
  opts <- setParams(opts, defaultOpts(), keepNotMatching = TRUE);
  # defaultOpts() fills in sensible defaults for all other controls
  # See tdmOptsDefaults.r for the list of those elements and many 
  # explanatory comments.  
  # Keep all elements present in opts, but NULL in defaultOpts().
}

controlSC <- function() {
  #
  # settings for the tuning process (former cpu_01.roi and .conf file):
  #
  ctrlSC = list(alg.roi=data.frame(lower=c(0.9, 3),
                                 upper=c(1.0, 5),
                                 type=c("FLOAT","INT"),
                                 row.names=c("XPERC","MTRY"))
              ,funEvals = 30
              ,designControl.size = 10
              ,optimizerControl.retries = 2    # optimLHD retries  (former seq.design.retries)
              ,replicates = 2
              ,noise = TRUE
              ,sCName = "cpu_01.conf"
  );
  
  ctrlSC <- setParams(ctrlSC,defaultSC());  
  # defaultSC() fills in sensible defaults for all other controls
  ctrlSC;
}


###################################################################
### start of main in demo04cpu
###################################################################

ctrlSC <- controlSC();
ctrlSC$opts <- controlDM();

# perform a complete tuning + unbiased eval
# 
envT <- tdmEnvTMakeNew(tdm,sCList=list(ctrlSC));# construct envT from settings given in tdm & sCList
dataObj <- tdmReadTaskData(envT,envT$tdm);
envT <- tdmBigLoop(envT,dataObj=dataObj);     # start the big tuning loop 


