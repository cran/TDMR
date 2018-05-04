#*# --------- demo/demo01cpu.r ---------
#*# This demo shows a simple data mining process (phase 1 of TDMR) for the regression task
#*# CPU (from UCI repository, http://archive.ics.uci.edu/ml/datasets/Computer+Hardware).
#*# The data mining process is in main_cpu.r, which calls tdmRegressLoop and tdmRegress
#*# with Random Forest as the prediction model. 

## path is the dir with data and main_*.r file:
path <- paste(find.package("TDMR"), "demo01cpu",sep="/");
#path <- paste("../inst", "demo01cpu",sep="/");

source(paste(path,"main_cpu.r",sep="/"));     # needed to define readCmdCpu

controlDM <- function() {
  #
  # settings for the DM process (former cpu_00.apd file): 
  # (see ?tdmOptsDefaultsSet for a complete list of all default settings 
  # and many explanatory comments)
  #
  opts = list(path = path,
              dir.data = "data/",
              filename = "cpu.csv",
              READ.TrnFn = readTrnCpu,    # defined in main_sonar.r
              TST.valiFrac=0.2,   # set this fraction of data aside for validation (only for DO.CV=F)
              TST.SEED = NULL,    # [NULL] a seed for the random test set selection
              NFOLD = 5,          # how many cross validation folds
              data.title = "CPU Data",
              MOD.method="RF",    # ["RF"|"MC.RF"|"SVM"|"NB" ...]
              OCUT = 600,         # cut records with output > OCUT (may be strong outliers, 
              # dropping them makes rmse$test and rmse$OOB faster
              # converge)
              SRF.kind = "ndrop",
              SRF.ndrop =  2 ,    # 0..n: how many variables (those with lowest importance) to drop
              RF.ntree = 50,
              RF.samp = 1000,
              RF.mtry = 3,
              fct.postproc="cpu.postproc",
              rgain.type="rmae",  # ["rmae" (default) |"rmse" ]
              gr.log=TRUE,        # if =T: log(x+1)-transform for graphics "true vs. predicted"   
              NRUN =  1,          # how many runs with different train & test samples  - or - 
              # how many CV-runs, if TST.kind="cv"
              GD.DEVICE="non",    # ["pdf"|"win"|"non"]: all graphics to 
              # [one multi-page PDF | (several) windows (X11) | dev.null]
              GD.RESTART=F,
              VERBOSE = 2,
              SRF.verbose = 1,
              logFile=FALSE       # no logfile (needed for Sweave/.Rnw only)
  );
  
  opts <- setParams(opts, defaultOpts(), keepNotMatching = TRUE);
  # defaultOpts() fills in sensible defaults for all other controls
  # See tdmOptsDefaults.r for the list of those elements and many 
  # explanatory comments.  
  # Keep all elements present in opts, but NULL in defaultOpts().
}

opts <- controlDM();
result <- main_cpu(opts);



