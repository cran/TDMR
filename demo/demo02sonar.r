require("TDMR");
oldwd <- getwd();
setwd(paste(.find.package("TDMR"), "demo02sonar",sep="/"));
tdm <- list(unbiasedFunc="unbiasedBestRun_C"
            , mainFile="main_sonar.r" # in working dir
            , mainCommand="result <- main_sonar(opts)"
            , tuneMethod=c("spot")    # other choices: "cmaes", "bfgs"
            , umode=c("CV")           # ["CV" | "RSUB" | "TST"]
            , finalFile="sonar.fin"   # where to write final results (best solution & unbiased eval for each tuner/.conf-combination)
            , experFile=NULL          # where to append final results
            , nrun=5, nfold=2         # repeats and CV-folds for the unbiased runs
            , optsVerbosity=0         # the verbosity for the unbiased runs
            , withParams=TRUE         # list the columns with tuned parameter in final results 
            , nExperim=1
            , parallelCPUs = 1        # [1] 1: sequential, >1: parallel with snowFall and this many cpus
            );
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
