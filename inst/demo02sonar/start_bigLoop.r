if (spotStep == "auto") {
  #
  # perform a complete tuning + unbiased eval
  # 
  envT <- tdmEnvTMakeNew(tdm); # construct envT from the TDMR settings given in tdm
  opts <- tdmEnvTGetOpts(envT,1);
  dataObj <- tdmSplitTestData(opts,tdm);
  envT <- tdmBigLoop(envT,spotStep,dataObj);  # start the big tuning loop
} else 
{ 
  # i.e. spotStep == "rep" or == "report"
  #
  # re-use prior tuning result; do only spot report and unbiased eval on best tuning solution
  # 
  if (!exists("envT")) 
    stop(sprintf("%s\n%s","Cannot run with spotStep=='rep' if there is no environment envT.",
                          "  Run this demo first with spotStep=='auto'"))
  load(envT$tdm$filenameEnvT);     # envT
  opts <- tdmEnvTGetOpts(envT,1);
  opts$READ.NROW=-1;          # or other settings which need to be different in case "rep"
  envT$tdm$optsVerbosity=0;
  dataObj <- tdmSplitTestData(opts,envT$tdm);
  envT <- tdmEnvTSetOpts(envT,opts);
  envT <- tdmTuneIt(envT,"rep",dataObj);
}

