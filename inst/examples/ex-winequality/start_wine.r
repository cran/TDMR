#*# Perform quick tuning run on a reduced sample from a larger dataset.
#*# This script is the first part of Lesson 8 in TDMR-tutorial.pdf,
#*# see there for further details

## path should point to a dir with subdir data/ and main_*.r file:
##
path <- paste(find.package("TDMR"), "examples/ex-winequality",sep="/");
#path <- paste("../..", "examples/ex-winequality",sep="/");

tdm=list(mainFile="main_wine.r"
         #,runList="wine_01.conf"     # save envT to <path>/wine_01.RData"
         ,filenameEnvT="wine_01.RData"   # file to save environment envT
         ,path=path
         ,umode="SP_T"
         ,U.saveModel=T
         ,optsVerbosity=1
         ,nrun=2
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
              filename = "winequality-white-train.csv",
              filetest = "winequality-white-test.csv",
              READ.TrnFn = readTrnWine,
              READ.NROW = 600,
              data.title = "Winequality Data",
              TST.SEED = 123456,
              
              RF.mtry = 4,
              RF.samp = 500,
              #opts$CLS.CLASSWT = c(10,10,10)
              #opts$CLS.cutoff = c(0.2,0.2,-1)
              
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
  # settings for the tuning process (former .roi and .conf file):
  #
  ctrlSC = list(alg.roi=data.frame(lower=c(0.9),
                                 upper=c(1.0),
                                 type=rep("FLOAT",1),
                                 row.names=c("XPERC"))
             ,funEvals = 8
             ,designControl.size = 2
             ,replicates = 2
             ,noise = TRUE
             ,sCName="wine_01.conf"
  );
  
  ctrlSC <- setParams(ctrlSC,defaultSC());  
                       # defaultSC() fills in sensible defaults for all other controls
  ctrlSC;
}


###################################################################
### start of main in start_wine
###################################################################

ctrlSC <- controlSC();
ctrlSC$opts <- controlDM();

# perform a complete tuning + unbiased eval
# 
envT <- tdmEnvTMakeNew(tdm,sCList=list(ctrlSC));# construct envT from settings given in tdm & sCList
dataObj <- tdmReadTaskData(envT,envT$tdm);    # read the task data from <path>/<data>

envT <- tdmTuneIt(envT,dataObj=dataObj);  # start the tuning loop  
print(envT$tdm$path)


