    path = paste(find.package("TDMR"), "demo02sonar",sep="/")
    envT = tdmEnvTLoad("demoSonar.RData",path);    # loads envT
#   envT = tdmEnvTLoad("demo03.RData",path);    # loads envT
    source(paste(path,"main_sonar.r",sep="/"));
    envT$tdm$nrun=0;       # =0: no unbiasedRun, >0: perform unbiasedRun with opts$NRUN=envT$tdm$nrun
    envT$sCList[[1]]$opts$VERBOSE=1;
    envT <- tdmEnvTReport(envT,1);
    if (!is.null(envT$theFinals)) print(envT$theFinals);
    

    
    