   oldwd <- getwd();
   setwd(paste(.find.package("TDMR"), "demo01cpu",sep="/"));
#   setwd(paste("../inst", "demo01cpu",sep="/"));
   tdm <- list(unbiasedFunc="unbiasedRun"
              , mainFile="main_cpu.r"   # in working dir
              , mainCommand="result <- main_cpu(opts)"
              , umode=c("RSUB","CV")    # ["CV" | "RSUB" | "TST"]
              , finalFile="cpu.fin"     # where to write final results (best solution & unbiased eval for each tuner/.conf-combination)
              , withParams=TRUE         # list the columns with tuned parameter in final results 
              , optsVerbosity=0         # the verbosity for the unbiased runs
              );
              
             # to give the user the chance to read the code above. After <Return>
             # the call   "envT <- tdmCompleteEval("cpu_01.conf",NULL,"auto",tdm);"
   plot(1);  # will start the whole TDMR-process with config file "cpu_01.conf"
   
   envT <- tdmCompleteEval("cpu_01.conf",NULL,"auto",tdm);
   setwd(oldwd);
