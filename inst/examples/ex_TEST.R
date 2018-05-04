#'   path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
#'   #path <- paste("../../inst", "demo02sonar",sep="/");
 
   ## control settings for TDMR
   tdm <- list( mainFunc="main_sonar"
              , runList = c("sonar_04.conf")
              , umode="CV"              # { "CV" | "RSUB" | "TST" | "SP_T" }
              , tuneMethod = c("lhd")
              , filenameEnvT="exBigLoop.RData"   # file to save environment envT 
              , nrun=1, nfold=2         # repeats and CV-folds for the unbiased runs
              , nExperim=1
              , optsVerbosity = 0       # the verbosity for the unbiased runs
              );
   source(paste(path,"main_sonar.r",sep="/"));    # main_sonar, readTrnSonar
   source(paste(path,"control_sonar.r",sep="/")); # controlDM, controlSC
   
   
   ctrlSC <- controlSC();
   ctrlSC$opts <- controlDM();
   
   # construct envT from settings given in tdm & sCList
   envT <- tdmEnvTMakeNew(tdm,sCList=list(ctrlSC));
   dataObj <- tdmReadTaskData(envT,envT$tdm);
   envT <- tdmBigLoop(envT,dataObj=dataObj);     # start the big tuning loop 
