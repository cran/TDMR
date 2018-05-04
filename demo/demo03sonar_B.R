#*# --------- demo/demo03sonar_B.r ---------
#*# Same as demo/demo03sonar.r, but with parameters for multiple tuning experiments & longer tuning runs:
#*#    in demo03sonar_B.r:    tdm$nExperim=2; tdm$nrun=5;
#*#    and in sonar_05.conf:  auto.loop.nevals = 50; init.design.size = 10;
#*# and with two tuners SPOT and LHD in comparison.
#*#

## path is the dir with data and main_*.r file:
path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");

## preliminary settings for TDMR
tdm <- list( mainFile="main_sonar.r"
            #, runList = "sonar_05.conf"
            , umode=c("RSUB")           # ["CV" | "RSUB" | "TST" | "SP_T" ]
            , tuneMethod = c("spot","lhd")
            , filenameEnvT="demo03.RData"   # file to save environment envT (in working dir)
            , nrun=5, nfold=2         # repeats and CV-folds for the unbiased runs
            , nExperim=2
            , parallelCPUs=1
            , parallelFuncs="readTrnSonar"  # fct's to export in addition to tdm$mainFunc in case parallelCPUs>1
            , optsVerbosity = 0       # the verbosity for the unbiased runs
            );
## Each element of envT$sCList has the settings for one tuning process (e.g. 
##    - funEvals = budget of model building runs and 
##    - alg.roi = data frame with ROI (region of interest)
##    - and so on
## ). 

source(paste(path,tdm$mainFile,sep="/"));    

controlDM <- function() {
  #
  # settings for the DM process (former sonar_05.apd & sonar_06.apd file): 
  #
  opts = list(path = path,
              dir.data = "data/",
              filename = "sonar.txt",
              READ.TrnFn = readTrnSonar,    # defined in main_sonar.r
              READ.NROW=100,
              data.title = "Sonar Data",
              TST.SEED = 124,
              MOD.SEED = 124,
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

controlSC05 <- function() {
  #
  # settings for the tuning process (former sonar_04.roi and .conf file):
  #
  ctrlSC = list(alg.roi=data.frame(lower=c(0.0, 5,0.5),
                                 upper=c(0.1,15,1.0),
                                 type=rep("FLOAT",3),
                                 row.names=c("CUTOFF1","CLASSWT2","XPERC"))
              ,funEvals = 50
              ,designControl.size = 4
              ,optimizerControl.retries = 2    # optimLHD retries  (former seq.design.retries)
              ,replicates = 5
              ,noise = TRUE
              ,sCName  ="sonar_05.conf"
  );
  
  ctrlSC <- setParams(ctrlSC,defaultSC());  
  # defaultSC() fills in sensible defaults for all other controls
  ctrlSC;
}

###################################################################
### start of main in demo03sonar
###################################################################

ctrlSC05 <- controlSC05();
ctrlSC05$opts <- controlDM();


# perform a complete tuning + unbiased eval
# 
envT <- tdmEnvTMakeNew(tdm,sCList=list(ctrlSC05));# construct envT from settings given in tdm & sCList
dataObj <- tdmReadTaskData(envT,envT$tdm);
envT <- tdmBigLoop(envT,dataObj=dataObj);     # start the big tuning loop  


