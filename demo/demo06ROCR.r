#*# --------- demo/demo06ROCR.r ---------
#*# Run task SONAR with "area under ROC curve" as performance measure (rgain.type="arROC").
#*# Other settings are similar to demo00sonar.r (phase 1 of TDMR).
#*# Finally, ROC curve and lift chart are plotted
#*#
    oldwd=getwd(); setwd(paste(.find.package("TDMR"), "demo02sonar",sep="/"));
    source("main_sonar.r");
      
    ## set the elements of list opts. See ?tdmOptsDefaultsSet
    ## for a complete list of all default settings and explanatory comments
    opts = tdmOptsDefaultsSet();    
    opts$filename = "sonar.txt"
    opts$READ.CMD = "read.csv2(file=paste(opts$dir.data, filename, sep=\"\"), dec=\".\", sep=\",\",header=FALSE)"
    opts$data.title <- "Sonar Data";
    opts$rgain.type <- "arROC";
    result = main_sonar(opts);
    
    tdmGraphicNewWin(opts);
    cat("Area under ROC-curve for validation data set: ");
    print(tdmROCRbase(result));                                 # side effect: plot ROC-curve
    tdmGraphicNewWin(opts);
    cat("Area under lift curve for training data set: ");
    print(tdmROCRbase(result,dataset="training",typ="lift"));   # side effect: plot lift chart
    setwd(oldwd);
