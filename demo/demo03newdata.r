#*# --------- demo/demo03newdata.r ---------
#*# Given a previous tuning process with results stored in envT and saved to "demo03.RData",
#*# reload this envT, reuse the best parameter setting (spotStep="rep"), train with this a  
#*# new model, using the training data in dsetTrnVa(dataObj) and predict with it
#*# on new data dsetTest(dataObj)
#*#
#*# To have "demo03.RData" available, demo03sonar.r must be run first (e.g.
#*# with demo("demo03sonar",ask=F) ).
path <- paste(find.package("TDMR"), "demo02sonar",sep="/"); 
#path <- paste("../inst", "demo02sonar",sep="/"); 
oldwd <- getwd(); setwd(path);
envT <- tdmEnvTLoad("demo03.RData");
source("main_sonar.r");    # defines readTrnSonar
source("sonar_06.apd")     # opts (sets opts$READ.TrnFn = readTrnSonar)
opts$READ.NROW=-1;
dataObj <- tdmSplitTestData(opts,envT$tdm); # read data, needs readTrnSonar  
envT <- tdmBigLoop(envT,"rep",dataObj);
setwd(oldwd);  
