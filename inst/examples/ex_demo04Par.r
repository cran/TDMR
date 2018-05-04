#*# This demo is the same as demo04cpu.r, except that it is executed on 2 cluster nodes in parallel.
#*# The only differences to demo04cpu.r are:
#*#     a) tdm$parallelCPUs=2
#*#     b) tdm$parallelFuncs=c("readCmdCpu","cpu.postproc")
#*# The latter is only necessary if main_cpu.r contains extra functions besides tdm$mainFunc="main_cpu",
#*# in this case "cpu.postproc" and "readCmdCpu", which need to be clusterExport'ed to the nodes

## load package and set working directory
require(TDMR);
path <- paste(find.package("TDMR"), "demo01cpu/",sep="/");
#path <- paste("../../inst", "demo01cpu/",sep="/");

source(paste(path,"main_cpu.r",sep=""));

## preliminary settings for TDMR
tdm <- list(    mainFunc="main_cpu"
              #, path=path
              #, runList="cpu_01.conf"
              , umode="RSUB"    # ["CV" | "RSUB" | "TST" | "SP_T" ]
              , tuneMethod=c("spot","lhd")
              , filenameEnvT="demo04cpu.RData"   # file to save environment envT (in working dir)
              , withParams=TRUE         # list the columns with tuned parameter in final results
              , optsVerbosity=0         # the verbosity for the unbiased runs
              , parallelCPUs=2
              , parallelFuncs=c("readCmdCpu","cpu.postproc")
              );
## fill in other defaults for list tdm
tdm <- tdmDefaultsFill(tdm);

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

# perform a complete tuning + unbiased eval:

## construct an initial environment envT from the given TDMR settings in tdm
## (this contains also the fill-in of other defaults for tdm via
##      envT$tdm <- tdmDefaultsFill(tdm);
## )
envT <- tdmEnvTMakeNew(tdm,sCList=list(ctrlSC));# construct envT from settings given in tdm & sCList
dataObj <- tdmReadTaskData(envT,envT$tdm);

## the call to tdmBigLoop will start the whole TDMR process:
## - for each file in tdm$runList a complete DM tuning is started with each tuning method tdm$tuneMethod 
## - the best result from tuning is fed into an unbiased model build and evaluation run
## - results are printed and returned in envT$theFinals
## - more detailed results are in other elements of environment envT
envT <- tdmBigLoop(envT,dataObj=dataObj);     # start the big tuning loop 



