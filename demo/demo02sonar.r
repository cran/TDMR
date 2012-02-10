require("TDMR");
oldwd <- getwd();
setwd(paste(.find.package("TDMR"), "demo02sonar",sep="/"));
#   setwd(paste("../inst", "demo01sonar",sep="/"));
source("main_sonar.r");    # in working dir
tdm <- list( mainFunction="main_sonar
            , umode=c("CV")           # ["CV" | "RSUB" | "TST"]
            , finalFile="sonar.fin"   # where to write final results (best solution & unbiased eval for each tuner/.conf-combination)
            , nrun=5, nfold=2         # repeats and CV-folds for the unbiased runs
            , withParams=TRUE         # list the columns with tuned parameter in final results 
            , optsVerbosity = 0       # the verbosity for the unbiased runs
            );
tdm <- tdmDefaultsFill(tdm);            
runList = c("sonar_04.conf");     # file(s) with settings for the tuning process (e.g. "auto.loop.steps"=number of SPOT generations 
                                  # or "auto.loop.evals"=budget of model building runs);
                                  # other possible files are: "sonar_01.conf","sonar_02.conf","sonar_03.conf"
                                  
spotList = NULL; # list() #       #  =NULL: all in runList; =list(): none
spotStep = "auto";

             # to give the user the chance to read the code above. After <Return>
             # the call   "envT <- tdmCompleteEval(runList,spotList,spotStep,tdm);"
plot(1);     # will start the whole TDMR-process

envT <- tdmCompleteEval(runList,spotList,spotStep,tdm);

setwd(oldwd);
