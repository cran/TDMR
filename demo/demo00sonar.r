require("TDMR");
oldwd <- getwd();
setwd(paste(.find.package("TDMR"), "demo02sonar",sep="/"));
source("main_sonar.r");

      opts = tdmOptsDefaultsSet();    # set initial defaults for many elements of opts. See tdmOptsDefaults.r
                                      # for the list of those elements and many explanatory comments
      opts$filename = "sonar.txt"
      if (exists("GRAPHDEV")) opts$GRAPHDEV = GRAPHDEV;   # choices are ["win" | "pdf² | "non"], default = "win"
      result=main_sonar(opts);

setwd(oldwd);
