  #
  # perform a complete tuning + unbiased eval
  # 
  envT <- tdmEnvTMakeNew(tdm); # construct envT from the TDMR settings given in tdm
  opts <- tdmEnvTGetOpts(envT,1);
  dataObj <- tdmSplitTestData(opts,tdm);
  envT <- tdmBigLoop(envT,"auto",dataObj);  # start the tuning loop 

