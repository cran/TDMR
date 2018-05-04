#*# This demo shows a level-2 example (SPOT tuning on task SONAR)

## path is the dir with data and main_*.r file:
path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");

tdm=list(mainFile="main_sonar.r"
         ,path=path
         ,filenameEnvT="envTSonar.RData"  # file to save environment envT (in dir path)
         ,umode="SP_T"
         ,U.saveModel=FALSE
         ,tuneMethod="spot" # "spot", "cma_j", "lhd"
);
source(paste(path,tdm$mainFile,sep="/"));    


###################################################################
### helper funcs for control parameter
###################################################################

controlDM <- function() {
  #
  # settings for the DM process (former .apd file): 
  #
  opts = list(path = path,
              dir.data = "data/",
              filename = "sonar.txt",
              READ.TrnFn = readTrnSonar,    # defined in main_sonar.r
              data.title = "Sonar Data",
              TST.SEED = 999,
              MOD.SEED = 999,
              RF.mtry = 4,
              CLS.cutoff = c(0.5,-1),
              CLS.CLASSWT = c(10,10),
              NRUN =  1,          # how many runs with different train & test samples  - or - 
                                  # how many CV-runs, if TST.kind="cv"
              GD.DEVICE="non",    # ["pdf"|"win"|"non"]: all graphics to 
                                  # [one multi-page PDF | windows (X11) | dev.null]
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
  # settings for the tuning process (former .roi and .conf file):
  #
  ctrlSC = list(alg.roi=data.frame(lower=c(0.1, 5,0.9),
                                 upper=c(0.8,15,1.0),
                                 type=rep("FLOAT",3),
                                 row.names=c("CUTOFF1","CLASSWT2","XPERC"))
              ,funEvals = 20
              ,designControl.size = 4
              ,designControl.replicates = 2
              ,seq.merge.func = mean   # mean or min
              ,replicates = 2
              ,noise=TRUE
              ,sCName="sonar_2.conf"
  );
  
  ctrlSC <- setParams(ctrlSC,defaultSC());  
                         # defaultSC() fills in sensible defaults for all other controls
  ctrlSC;
}


###################################################################
### start of main in demo02sonar
###################################################################

ctrlSC <- controlSC();
ctrlSC$opts <- controlDM();

# perform a complete tuning + unbiased eval
# 
envT <- tdmEnvTMakeNew(tdm,sCList=list(ctrlSC));# construct envT from settings given in tdm & sCList
dataObj <- tdmReadTaskData(envT,envT$tdm);
envT <- tdmBigLoop(envT,dataObj=dataObj);     # start the big tuning loop 




