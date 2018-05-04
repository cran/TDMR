#*# --------- demo/demo03sonar.r ---------
#*# This demo shows a complete tuned data mining process (level 3 of TDMR) where 
#*# the data mining task is the classification task SONAR (from UCI repository, 
#*# http://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+%28Sonar,+Mines+vs.+Rocks%29).
#*# The data mining process is in main_sonar.r, which calls tdmClassifyLoop and tdmClassify
#*# with Random Forest as the prediction model. 
#*# The three parameter to be tuned are CUTOFF1, CLASSWT2 and XPERC, as specified 
#*# in file sonar_04.roi. The tuner used here is LHD.  
#*# Tuning runs are rather short, to make the example run quickly. 
#*# Do not expect good numeric results. 
#*# See demo/demo03sonar_B.r for a somewhat longer tuning run, with two tuners SPOT and LHD.

## path is the dir with data and main_*.r file:
path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");

## preliminary settings for TDMR
tdm <- list( mainFile="main_sonar.r"
            #, runList = c("sonar_04.conf","sonar_06.conf")
            , umode="CV"              # { "CV" | "RSUB" | "TST" | "SP_T" }
            , tuneMethod="spot"       # "spot", "cma_j", "lhd"
            , path=path
            , filenameEnvT="demo03.RData"   # file to save environment envT (in dir 'path')
            , nrun=3, nfold=2         # repeats and CV-folds for the unbiased runs
            , nExperim=1
            , parallelCPUs=1
            , parallelFuncs=c("readTrnSonar")
            , U.saveModel=T
            , optsVerbosity = 3       # the verbosity for the unbiased runs
            );
## Each element of envT$sCList has the settings for one tuning process (e.g. 
##    - funEvals = budget of model building runs and 
##    - alg.roi = data frame with ROI (region of interest)
##    - and so on
## ). 

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
              TST.SEED = 124,
              MOD.SEED = 124,
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

controlSC04 <- function() {
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
              ,nose = TRUE
              ,sCName="sonar_04.conf"
  );
  
  ctrlSC <- setParams(ctrlSC,defaultSC(),keepNotMatching = TRUE);  
  # defaultSC() fills in sensible defaults for all other controls
  ctrlSC;
}

controlSC06 <- function() {
  #
  # settings for the tuning process (former sonar_06.roi and .conf file):
  #
  ctrlSC = list(alg.roi=data.frame(lower=c(0.0, 5,0.5),
                                 upper=c(0.5,15,1.0),
                                 type=rep("FLOAT",3),
                                 row.names=c("CUTOFF1","CLASSWT2","XPERC"))
              ,funEvals = 20
              ,designControl.size = 4
              ,replicates = 2
              ,noise = TRUE
              ,sCName="sonar_06.conf"
  );
  
  ctrlSC <- setParams(ctrlSC,defaultSC());  
  # defaultSC() fills in sensible defaults for all other controls
  ctrlSC;
}

###################################################################
### start of main in demo03sonar
###################################################################

ctrlSC04 <- controlSC04();
ctrlSC04$opts <- controlDM();
ctrlSC06 <- controlSC06();
ctrlSC06$opts <- controlDM();

# perform a complete tuning + unbiased eval
# 
envT <- tdmEnvTMakeNew(tdm,sCList=list(ctrlSC04,ctrlSC06));# construct envT from settings given in tdm & sCList
dataObj <- tdmReadTaskData(envT,envT$tdm);
envT <- tdmBigLoop(envT,dataObj=dataObj);     # start the big tuning loop 


