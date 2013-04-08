#*# --------- demo/demo00sonar.r ---------
#*# This demo shows a simple data mining process (phase 1 of TDMR) for the classification task
#*# SONAR (from UCI repository, 
#*' http://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+%28Sonar,+Mines+vs.+Rocks%29).
#*# The data mining process is in main_sonar.r, which calls tdmClassifyLoop and tdmClassify.
#*# with Random Forest as the prediction model. 

## load package and set working directory
#require("TDMR");
path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");
oldwd <- getwd();
setwd(path);
source("main_sonar.r");              # contains also readCmdSonar()

source("sonar_00.apd",local=TRUE);   # set opts 

result=main_sonar(opts);

setwd(oldwd);
