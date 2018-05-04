#*# --------- demo/demo03newdata.r ---------
#*# Given a previous tuning process with results stored in envT and saved to "demo03.RData",
#*# reload this envT, reuse the best parameter setting, train with this a  
#*# new model, using the training data in dsetTrnVa(dataObj) and predict with it
#*# on new data dsetTest(dataObj)
#*#
#*# To have "demo03.RData" available, demo03sonar.r must be run first (e.g.
#*# with demo("demo03sonar",ask=F) ).
path <- paste(find.package("TDMR"), "demo02sonar",sep="/"); 
#path <- paste("../inst", "demo02sonar",sep="/"); 
envT <- tdmEnvTLoad("demoSonar.RData",path);
source(paste(path,"main_sonar.r",sep="/"));  
opts <- tdmEnvTGetOpts(envT);
opts$READ.NROW=-1;
opts$path <- path;
envT <- tdmEnvTSetOpts(envT,opts);
dataObj <- tdmReadAndSplit(opts,envT$tdm); # read data, needs readTrnSonar  
envT$tdm$nrun=1;       # =0: no unbiasedRun, >0: perform envT$tdm$nrun unbiased runs
envT <-  tdmEnvTReport(envT,1);
if (!is.null(envT$theFinals)) print(envT$theFinals); 

