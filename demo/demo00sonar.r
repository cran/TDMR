require("TDMR");
oldwd <- getwd();
setwd(paste(.find.package("TDMR"), "demo02sonar",sep="/"));
source("main_sonar.r");

      opts = tdmOptsDefaultsSet();    # set initial defaults for many elements of opts. See tdmOptsDefaults.r
                                      # for the list of those elements and many explanatory comments
      opts$filename = "sonar.txt"
      opts$ncopies=100;
      if (exists("GRAPHDEV")) opts$GD.DEVICE = GRAPHDEV;   # choices are ["win" | "pdf� | "non"], default = "win"
      result=main_sonar(opts);

setwd(oldwd);
