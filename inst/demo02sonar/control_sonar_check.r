controlDM <- function() {
  #
  # settings for the DM process (former sonar_04.apd file): 
  #
  opts = list(path = path,
              dir.data = "data/",
              filename = "sonar.txt",
              READ.TrnFn = readTrnSonar,    # defined in main_sonar.r
              data.title = "Sonar Data",
              TST.SEED = 124,
              MOD.SEED = 124,
              READ.NROW= -1,
              CLS.cutoff = c(0.9,-1),
              SRF.cutoff = c(0.9,-1),
              SRF.kind="xperc",
              CLS.CLASSWT = c(10,10),
              NRUN =  1,          
              GD.DEVICE="non",    
              GD.RESTART=FALSE,
              VERBOSE = 0,
              SRF.verbose = 0
  );
  opts <- setParams(opts, defaultOpts(), keepNotMatching = TRUE);
}

controlSC <- function() {
  #
  # settings for the tuning process (former sonar_04.roi and .conf file):
  #
  ctrlSC = list(alg.roi=data.frame(lower=c(0.0, 5,0.5),
                                 upper=c(0.1,15,1.0),
                                 type=rep("FLOAT",3),
                                 row.names=c("CUTOFF1","CLASSWT2","XPERC"))
              ,funEvals = 10
              ,designControl.size = 2
              ,seq.merge.func = mean   # mean or min
              ,replicates = 2
              ,noise = TRUE
              ,sCName = "sonar_04.conf"
  );

  ctrlSC <- setParams(ctrlSC,defaultSC());  
  # defaultSC() fills in sensible defaults for all other controls
  ctrlSC;
}
