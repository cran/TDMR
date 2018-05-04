#*# --------- demo/demo07parallel.r ---------
#*# This demo does the same as demo03sonar.r, but it runs 8 experiments on 4 parallel cores
#*# (if your environment supports parallel clusters with the R-core package 'parallel'). 
#*#
#*# Note on your task manager, how for a short time 4 processes are active (usually 'RScript').
#*# 
#*# Tuning runs are rather short, to make the example run quickly. 
#*# Do not expect good numeric results. 
#*# See demo/demo03sonar_B.r for a somewhat longer tuning run, with two tuners SPOT and LHD.

## path is the dir with data and main_*.r file:
path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");

## preliminary settings for TDMR
tdm <- list( mainFile="main_sonar.r"
            #, runList = "sonar_04.conf"
            , umode=c("CV")           # ["CV" | "RSUB" | "TST" | "SP_T" ]
            , tuneMethod =  c("spot") # c("spot","lhd")   
            , filenameEnvT="demo03p.RData"   # file to save environment envT (in working dir)
            , nrun=2, nfold=2         # repeats and CV-folds for the unbiased runs
            , nExperim=4
            , parallelCPUs=4
            , parallelFuncs=c("readTrnSonar")
            , optsVerbosity = 0       # the verbosity for the unbiased runs
            );
source(paste(path,tdm$mainFile,sep="/"));   

###################################################################
### helper funcs for control parameter
###################################################################

controlDM <- function() {
  #
  # settings for the DM process (former sonar_04.apd file): 
  #
  opts = list(path = path,
              dir.data = "data/",
              filename = "sonar.txt",
              READ.TrnFn = readTrnSonar,    # defined in main_sonar.r
              READ.NROW=100,
              data.title = "Sonar Data",
              TST.SEED = 125,
              MOD.SEED = NULL, #125,
              #RF.mtry = 4,             # this would lead to 'invalid mtry' warnings
              CLS.cutoff = c(0.9,-1),
              SRF.cutoff = c(0.9,-1),
              #CLS.CLASSWT = c(10,10),
              NRUN =  1,          # how many runs with different train & test samples  - or - 
              # how many CV-runs, if TST.kind="cv"
              GD.DEVICE="non",    # ["pdf"|"win"|"non"]: all graphics to 
              # [one multi-page PDF | (several) windows (X11) | dev.null]
              GD.RESTART=F,
              VERBOSE = 0,
              SRF.verbose = 0,
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
  # settings for the tuning process (former sonar_04.roi and .conf file):
  #
  ctrlSC = list(alg.roi=data.frame(lower=c(0.0, 5,0.5),
                                 upper=c(0.1,15,1.0),
                                 type=rep("FLOAT",3),
                                 row.names=c("CUTOFF1","CLASSWT2","XPERC"))
              ,funEvals = 20
              ,designControl.size = 4
              ,optimizerControl.retries = 2    # optimLHD retries  (former seq.design.retries)
              ,replicates = 2
              ,noise = TRUE
              ,sCName="sonar_04.conf"
  );
  
  ctrlSC <- setParams(ctrlSC,defaultSC());  
  # defaultSC() fills in sensible defaults for all other controls
  ctrlSC;
}

###################################################################
### start of main 
###################################################################

ctrlSC <- controlSC();
ctrlSC$opts <- controlDM();

# perform a complete tuning + unbiased eval
# 
envT <- tdmEnvTMakeNew(tdm,sCList=list(ctrlSC));# construct envT from settings given in tdm & sCList
dataObj <- tdmReadTaskData(envT,envT$tdm);
envT <- tdmBigLoop(envT,dataObj=dataObj);     # start the big tuning loop 


