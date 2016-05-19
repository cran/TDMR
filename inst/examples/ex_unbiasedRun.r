    ## Load the best results obtained in a prior tuning for the configuration "sonar_04.conf" with tuning method "spot".
    ## The result envT from a prior run of tdmBigLoop with this .conf is read from demo02sonar/demoSonar.RData.
    ## Run task main_sonar again with these best parameters, using the default settings from tdmDefaultsFill
    ## umode="RSUB", tdm$nrun=5  and tdm$TST.testFrac=0.2.
    oldwd <- getwd();
    ## The best results are read from demo02sonar/demoSonar.RData relative to the TDMR package directory.
    setwd(paste(find.package("TDMR"), "demo02sonar",sep="/"));
    #setwd("../demo02sonar")
    load("demoSonar.RData");
    opts <- tdmEnvTGetOpts(envT,1);
    envT$tdm$optsVerbosity=1;
    dataObj <- tdmSplitTestData(opts,envT$tdm);
    source("main_sonar.r");
    finals <- unbiasedRun("sonar_04.conf",envT,dataObj,tdm=envT$tdm);
    print(finals);
    setwd(oldwd);

    #    --- The following example is now deprecated, because we do not support .bst-files any longer ----
    #    \dontrun{
    #    ## If you do not have 'envT' but only a .bst file from a prior tuning run:
    #    ## The best results are read from demo02sonar/spot/sonar_04.bst relative to the TDMR package directory.
    #    ## (This example is not run automatically, because sonar_04.bst is not in the package distribution) 
    #    setwd(paste(find.package("TDMR"), "demo02sonar",sep="/"));
    #    envT <- new.env();
    #    tdm <- list(mainFunc="main_sonar", tuneMethod="spot");
    #    source("main_sonar.r");
    #    finals <- unbiasedRun("sonar_04.conf",envT,tdm=tdm);
    #    print(finals);
    #    setwd(oldwd);
    #    }
    
    
    