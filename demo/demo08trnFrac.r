# trSetSizeTDMR.R
# Exp. Setup:
# - Sonar Dataset
# - 66% of the Sonar data available for training, rest is test data (test data is fixed 
#   in the inner loop but resampled in each outer iteration (training step sizes are varied 10 times))
# - SVM is chosen as model, tunable parameters are the RBF kernel parameters 'gamma' and 'C'
# - training set size is increased in 5% steps varying from 5 and 95% of the 80% patterns of the Sonar dataset
# - SPOT is used for parameter tuning, ranges are gamma [0, 1] and C in [0, 10]. Parameter is initialized with [0.1, 1.0]
# - termination criterion is 50 iterations, ~150 function evaluations per setting (see .conf)
#library(mlbench)
#library(e1071)
#library(cmaes)

library(TDMR)
#
# source TDMR and SPOT from code to allow debugging
#---- see below the 2  lines after definition of tdm

path <- paste("../inst", "demo08trnFrac/",sep="/");
oldwd <- getwd();
setwd(path);



# initialization
set.seed(1)
  name = "sonar"
  target <- "Class"

#' Evaluation function
#' a SVM model is trained for training data (a fraction xperc of the train-vali-data in 'dset') and 
#' evaluated on independent test data in 'tset'.
#' Parameters which need to be def'd in the environment of mainTrnFrac
#'    xperc
#'    target
#'    
#' Tuned parameters: x vector containing SVM parameters gamma x[1] and Cost x[2]
#' @return 'result', a list containing classification error
mainTrnFrac <- function(opts,dset=NULL,tset=NULL){
  if (is.null(dset)) stop("dset is NULL");
  response.variable <- c(target)
  input.variables <- setdiff(colnames(dset),c(target,opts$TST.COL));
  opts <- tdmOptsDefaultsSet(opts);  # fill in all opts params which are not yet set (see tdmOptsDefaults.r)
  #opts$VERBOSE=2
  opts$TST.trnFrac=xperc
  opts$TST.valiFrac=1-xperc
  gdObj <- tdmGraAndLogInitialize(opts)     

  result <- tdmClassifyLoop(dset,response.variable,input.variables,opts,tset);

  # print summary output and attach certain columns (here: y,sd.y,dset) to list result:
  result <- tdmClassifySummary(result,opts);
  tdmGraAndLogFinalize(opts,gdObj);      # close graphics and log file 
  
  return(result)
}



readSonar <- function(filename, opts) {
    data(Sonar);
    dset <- Sonar;
}
# 
# Where is this read-function used? - This is specified in the .apd file associated with .conf file runList[1], e.g.
#     opts$READ.CMD = "readSonar(filename, opts)"
# in sonar_01.apd.
# runList[1] is set below as  paste(name,"_01.conf",sep="")
#

# TDM specific settings
tdm <- list(  unbiasedFunc="unbiasedRun"
            , umode=c("SP_T")
            , mainFile=NULL#""
            , mainFunction="mainTrnFrac"
            , tuneMethod=c("spot")   #   ,  "cma_j"   "cmaes"   "bfgs"     ,     
            , finalFile=paste("Output/",name,".fin",sep="")
            , nrun=1, nfold=2          # repeats and CV-folds for the unbiased runs
            , optsVerbosity=0           # the verbosity for the unbiased runs
            , withParams=T
            , fileMode=TRUE
            , nExperim= 1              # number of tuning experiments, results e.g. in spot/sonar_01_spot_01.bst, spot/sonar_01_spot_02.bst, ... 
            , parallelCPUs = 1         # [1] 1: sequential, >1: parallel with snowFall and this many cpus
            #, path=path
            , tdmPath=NULL #  "../../../TDMR" # NULL #  source R-files for TDMR from this dir. If NULL, load instead the library TDMR 
            , theSpotPath = NA # "USE.SOURCE"
            );

#-- the following 2 lines are only needed if you want to run the developer sources in tdm$tdmPath (and optionally the SPOT sources)  
#-- or if you need to initiate parallel execution; otherwise a simple  "require(TDMR);"  will do.
start.tdm.path <- ifelse(is.null(tdm$tdmPath),.find.package("TDMR"),paste(tdm$tdmPath,"inst",sep="/")); 
source(paste(start.tdm.path,"start.tdm.r",sep="/"),local=T); 
#require(TDMR);

tdm$runList = c(paste(name,"_01.conf",sep=""));  
tdm$spotList = NULL # list() #       #  =NULL: all in tdm$runList; =list(): none
spotStep = "rep"
if (tdm$parallelCPUs>1) sfExport(list=c("Sonar","readSonar"));

dir.output <- "Results2013";
if (!file.exists(dir.output)) {
  success = dir.create(dir.output);     
  if (!success) stop(sprintf("Could not create dir.output=%s",dir.output));
}
tdm$filenameEnvT0 =  paste( dir.output,"/" , name, "-", sprintf("%03d",1), "-", 1, ".RData", sep=""); 


###########
# main loop
############
dfFinal=NULL
for(nruns in 1:2){       # now we do 10 experiments via tdm$nExperim

  tdm$SPLIT.SEED = 42; # tdmRandomSeed();
  # As long as tdm$SPLIT.SEED is the same (here: during xperc-loop), the same test 
  # data set will be set aside by tdmSplitTestData
  

  j=1                    # j is loop variable similar to xperc and just required for the file name description
  for(xperc in seq(0.05,0.75,0.10)){      # dfFinalA.RData

    tdm$TST.trnFrac=xperc;          # trnFrac of train-vali-set is trnFrac*(1-20%) of all data (if opts$TST.testFrac=20%)
    valiPerc = 1-xperc;
    # Note: As long as opts$TST.SEED in .apd is set to a fixed value, we will have the
    # same split into training and validation data during all tuning evaluations within this for-loop.
    # If opts$TST.SEED is NULL (undef'd), each split into training and validation data is different.

    # prepare for saving of reduced envT on tdm$filenameEnvT (in tdmBigLoop)
    tdm$filenameEnvT=paste( dir.output,"/" , name, "-", sprintf("%03d",nruns), "-", j, ".RData", sep="");     
    print(tdm$filenameEnvT);      
    
    envT <- tdmEnvTMakeNew(tdm);    # contains "tdm <- tdmDefaultsFill(tdm)"
    envT <- tdmEnvTAddBstRes(envT,tdm$filenameEnvT0);     # fill in envT$bstGrid & envT$resGrid (needed for spotStep="rep")
    envT <- tdmBigLoop(envT,spotStep);

    #cma.values[nruns,j] <- envT$theFinals$RGain.TST
    print(envT$theFinals$RGain.TST)
      
    class.error <- envT$theFinals$RGain.TST #TODO mean error on independent test set
    dfFinal = rbind(dfFinal,cbind(envT$theFinals,nrun=nruns,xperc=xperc))
    
      j = j + 1 

  }#for(xperc)

}#for (nruns) 

dfFinal=dfFinal[order(dfFinal$xperc),];
print(dfFinal);
filenameDfFinal=paste( dir.output,"/dfFinalA-" , name, ".RData", sep="");     
save(dfFinal,file=filenameDfFinal);


## restore old working directory
setwd(oldwd);
