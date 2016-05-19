## working directory is this dir (dir with .apd, .conf and main_*.r file):

tdm=list(mainFile="main_wine.r"
         ,runList="wine_01.conf"  # in "wine_01.apd": opts$READ.NROW=600
         ,umode="SP_T"
         ,U.saveModel=F
         ,optsVerbosity=1
         ,nrun=2
);
source(tdm$mainFile);  

#
# perform a complete tuning + unbiased eval
# 
envT <- tdmEnvTMakeNew(tdm); # construct envT from the TDMR settings given in tdm
opts <- tdmEnvTGetOpts(envT,1);
dataObj <- tdmSplitTestData(opts,tdm);
envT <- tdmTuneIt(envT,"auto",dataObj);  # start the tuning loop


