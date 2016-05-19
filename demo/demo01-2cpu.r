#*# --------- demo/demo01cpu.r ---------
#*# This demo shows a simple data mining process (phase 1 of TDMR) for the regression task
#*# CPU (from UCI repository, http://archive.ics.uci.edu/ml/datasets/Computer+Hardware).
#*# The data mining process is in main_cpu.r, which calls tdmRegressLoop and tdmRegress
#*# with Random Forest as the prediction model. 

## load package and set working directory
#library(TDMR);
path <- paste(find.package("TDMR"), "demo01cpu",sep="/");
#path <- paste("../inst", "demo01cpu",sep="/");

source(paste(path,"main_cpu.r",sep="/"));     # needed to define readCmdCpu
source(paste(path,"cpu_00.apd",sep="/"),local=TRUE);   # set opts, needs readCmdCpu 
source(paste(path,"start_cpu.r",sep="/"),chdir=TRUE,print.eval=TRUE);  # contains: result<-main_sonar(opts);



