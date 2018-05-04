#*# --------- demo/demo06cma_j.r ---------
#*# This demo shows for tuner cma_j (CMA-ES, Java version via package rCMA) a complete tuned data mining process (TDMR, level 3). 
#*# Other settings are the same as in demo03sonar.r, except that we use sonar_03.conf as configuration file.
#*# 

require(rJava);     # this is needed since we have rCMA only on the Suggest list

path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");

## preliminary settings for TDMR
tdm <- list(  mainFunc="main_sonar"
            #, runList = "sonar_03.conf"
            , umode=c("RSUB")           # ["CV" | "RSUB" | "TST" | "SP_T" ]
            , tuneMethod = c("cma_j","spot")       # "cma_j","spot"
            , path=path
            , filenameEnvT="demoSonarCma_j.RData"   # file to save environment envT 
            , nrun=5, nfold=2         # repeats and CV-folds for the unbiased runs
            , nExperim=1 #2
            , parallelCPUs=1
            #, parallelFuncs=c("readCmdSonar")
            , optsVerbosity = 0       # the verbosity for the unbiased runs
            , U.saveModel = FALSE
            , CMA.populationSize = 3  # the CMA population size. If not set, take the default 4+3*log(N)
);
source(paste(path,"main_sonar.r",sep="/"));    

###################################################################
### helper funcs for control parameter
###################################################################

controlDM <- function() {
  #
  # settings for the DM process (former sonar_03.apd file): 
  #
  opts = list(path = path,
              dir.data = "data/",
              filename = "sonar.txt",
              READ.TrnFn = readTrnSonar,    # defined in main_sonar.r
              data.title = "Sonar Data",
              TST.SEED = 124,
              MOD.SEED = 124,
              RF.mtry = 4,
              CLS.cutoff = c(0.5,-1),
              CLS.CLASSWT = c(10,10),
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
  # settings for the tuning process (former sonar_03.roi and .conf file):
  #
  ctrlSC = list(alg.roi=data.frame(lower=c(0.1, 5,0.9),
                                 upper=c(0.8,15,1.0),
                                 type=rep("FLOAT",3),
                                 row.names=c("CUTOFF1","CLASSWT2","XPERC"))
              ,funEvals = 50
              ,designControl.size = 10
              ,designControl.replicates = 2
              ,seq.merge.func = mean   # mean or min
              ,replicates = 2
              ,noise = TRUE
              ,sCName="sonar_03.conf"
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

