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
   #path <- paste("../../inst", "demo02sonar",sep="/");
 
   ## control settings for TDMR
   tdm <- list( mainFunc="main_sonar"
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

   #
   # perform a complete tuning + unbiased eval
   # 
   envT <- tdmEnvTMakeNew(tdm,sCList=list(ctrlSC)); # construct envT from settings in tdm and ctrlSC
   dataObj <- tdmReadTaskData(envT,envT$tdm);
   envT <- tdmTuneIt(envT,dataObj=dataObj);       # start the tuning loop 
   cat("Deleting exBigLoop.RData again\n")
   unlink(paste(envT$tdm$path,"exBigLoop.RData",sep="/"));
   cat("Deleting Output again\n")
   unlink(paste(envT$tdm$path,"Output",sep="/"),recursive=TRUE);
   
   