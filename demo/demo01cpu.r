#*# --------- demo/demo01cpu.r ---------
#*# This demo shows a simple data mining process (phase 1 of TDMR) for the regression task
#*# CPU (from UCI repository, http://archive.ics.uci.edu/ml/datasets/Computer+Hardware).
#*# The data mining process is in main_cpu.r, which calls tdmRegressLoop and tdmRegress
#*# with Random Forest as the prediction model. 

## load package and set working directory
library(TDMR);
path <- paste(.find.package("TDMR"), "demo01cpu",sep="/");
#path <- paste("../inst", "demo01cpu",sep="/");
oldwd <- getwd();
setwd(path);
source("main_cpu.r");   # in working dir 

## set the elements of list opts. See ?tdmOptsDefaultsSet
## for a complete list of all default settings and explanatory comments
opts = tdmOptsDefaultsSet();    
      opts$dir.output <- paste("Output/", sep="")
      opts$filename = "cpu.csv"
      opts$READ.CMD = "read.csv2(file=paste(dir.data, filename, sep=\"\"), dec=\".\", na.string=\"-1\",nrow=opts$READ.NROW)"
      opts$SRF.kind = "ndrop"
      opts$SRF.ndrop =  2     # 0..n: how many variables (those with lowest importance) to drop
      opts$OCUT = 600         # cut records with output > OCUT (may be strong outliers, dropping
                              # them makes rmse$test and rmse$OOB faster converge)
      opts$fct.postproc <- "cpu.postproc";

      opts$gr.log=TRUE        # if =T: log(x+1)-transform for graphics "true vs. predicted"   
      opts$GD.DEVICE="win"    # ="pdf": all graphics to one multi-page PDF
                              # ="win": all graphics to (several) windows (X11)
      opts$data.title <- "CPU Data";
      opts$VERBOSE=2;      
      
result=main_cpu(opts);

## restore old working directory
setwd(oldwd);

