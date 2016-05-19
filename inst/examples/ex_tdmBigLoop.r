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

## set working directory (dir with .apd, .conf and main_*.r file)
path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
source(paste(path,"main_sonar.r",sep="/"));    

## control settings for TDMR
tdm <- list( mainFunc="main_sonar"
           , runList = c("sonar_04.conf")
           , umode="CV"              # { "CV" | "RSUB" | "TST" | "SP_T" }
           , tuneMethod = c("lhd")
           , filenameEnvT="exBigLoop.RData"   # file to save environment envT (in dir 'path')
           , nrun=1, nfold=2         # repeats and CV-folds for the unbiased runs
           , nExperim=1
           , parallelCPUs=1
           , parallelFuncs=c("readCmdSonar")
           , optsVerbosity = 0       # the verbosity for the unbiased runs
           );
## Each element of tdm$runList has the settings for one tuning process (e.g. 
##    - auto.loop.steps = number of SPOT generations       
##    - auto.loop.evals = budget of model building runs and 
##    - io.roiFileName = "sonar_04.roi"
## ). 

spotStep = "auto";   
source(paste(path,"start_bigLoop.r",sep="/"),chdir=TRUE);    # change dir to 'path' while sourcing


