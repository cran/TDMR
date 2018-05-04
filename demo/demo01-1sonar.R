#*# --------- demo/demo00sonar.r ---------
#*# This demo shows a simple data mining process (level 1 of TDMR) for the classification task
#*# SONAR (from UCI repository, 
#*# http://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+%28Sonar,+Mines+vs.+Rocks%29).
#*# The data mining process is in main_sonar.r, which calls tdmClassifyLoop and tdmClassify.
#*# with Random Forest as the prediction model. 

## path is the dir with data and main_*.r file:
path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
#path <- paste("../inst", "demo02sonar",sep="/");

source(paste(path,"main_sonar.r",sep="/"));     # needed to define readTrnSonar

controlDM <- function() {
  #
  # settings for the DM process (former sonar_00.apd file): 
  # (see ?tdmOptsDefaultsSet for a complete list of all default settings 
  # and many explanatory comments)
  #
  opts = list(path = path,
              dir.data = "data",            # relative to path
              filename = "sonar.txt",
              READ.TrnFn = readTrnSonar,    # defined in main_sonar.r
              data.title = "Sonar Data",
              NRUN =  1,           # how many runs with different train & test samples  - or - 
                                  # how many CV-runs, if TST.kind="cv"
              VERBOSE = 2
  );
  
  opts <- setParams(opts, defaultOpts(), keepNotMatching = TRUE);
                          # defaultOpts() fills in sensible defaults for all other controls
                          # See tdmOptsDefaults.r for the list of those elements and many 
                          # explanatory comments.  
                          # Keep all elements present in opts, but NULL in defaultOpts().
}

opts <- controlDM();
result <- main_sonar(opts); 




