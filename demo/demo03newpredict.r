#*# --------- demo/demo03newpredict.r ---------
#*# Given a previous tuning process with results stored in envT and saved to "demo03.RData",
#*# reload this envT, reuse the last model (envT$result$lastRes$lastModel) and predict with it
#*# on new data --> predicted output z.
#*#
#*# To have "demo03.RData" available, demo03sonar.r must be run first (e.g.
#*# with demo("demo03sonar",ask=F) ). To have lastModel available, the prior tuning process must
#*# have been done with tdm$U.saveModel==TRUE (which is the default)
#*#
path <- paste(find.package("TDMR"), "demo02sonar",sep="/"); 
#path <- paste("../inst", "demo02sonar",sep="/"); 

oldwd <- getwd(); setwd(path);
envT <- tdmEnvTLoad("demo03.RData");
source("main_sonar.r");    # defines readTrnSonar
source("sonar_06.apd")     # opts (sets opts$READ.TrnFn = readTrnSonar)
opts$READ.NROW=-1;
dataObj <- tdmSplitTestData(opts,envT$tdm); # read data, needs readTrnSonar  
newdata <- dataObj$dset[1:50,];
z=predict(envT,newdata);
print(z);
setwd(oldwd); 
