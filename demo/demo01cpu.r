   oldwd <- getwd();
   setwd(paste(.find.package("TDMR"), "demo01cpu",sep="/"));
#   setwd(paste("../inst", "demo01cpu",sep="/"));
   source("main_cpu.r");   # in working dir 
   tdm <- list( mainFunction="main_cpu"
              , umode=c("RSUB","CV")    # ["CV" | "RSUB" | "TST"]
              , finalFile="cpu.fin"     # where to write final results (best solution & unbiased eval for each tuner/.conf-combination)
              , withParams=TRUE         # list the columns with tuned parameter in final results 
              , optsVerbosity=0         # the verbosity for the unbiased runs
              );
   tdm <- tdmDefaultsFill(tdm);            
browser()              
              
             # to give the user the chance to read the code above. After <Return>
             # the call   "envT <- tdmCompleteEval("cpu_01.conf",NULL,"auto",tdm);"
   plot(1);  # will start the whole TDMR-process with config file "cpu_01.conf"
   
   envT <- tdmCompleteEval("cpu_01.conf",NULL,"auto",tdm);
   setwd(oldwd);
