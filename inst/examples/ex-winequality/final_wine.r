#*# Perform high-quality training & eval on all data for a reloaded parameter configuration.
#*# This script is the second part of Lesson 8 in TDMR-tutorial.pdf,
#*# see there for further details.

## path should point to a dir with subdir data/, main_*.r, and .Rdata file:
##
path <- paste(find.package("TDMR"), "examples/ex-winequality",sep="/");
#path <- paste("../..", "examples/ex-winequality",sep="/");

tdm=list(mainFile="main_wine.r"
         ,path=path
         ,filenameEnvT="wine_01.RData"   # reload envT from <path>/wine_01.RData"
         ,umode="TST"
         ,TST.valiFrac=0
         ,U.saveModel=T
         ,optsVerbosity=1
         ,nrun=2
);
source(paste(path,tdm$mainFile,sep="/"));  

#
# re-use prior tuning result from envT: do only tdmEnvTReport and unbiased eval on 
# best tuning result. But do so by training a model on all training data 
# (80% of 4898 =3919 records: white-wine-train.csv) and testing it on all test data 
# (20% of 4898 = 979 records: white-wine-tst.csv).
# 
tdm <- tdmDefaultsFill(tdm)
envT<- tdmEnvTLoad(tdm$filenameEnvT,path);    # loads envT

envT<- tdmEnvTUpdate(envT,tdm); 
                    # update the re-loaded envT$tdm with new elements given in tdm

opts <- tdmEnvTGetOpts(envT);
opts$READ.NROW=-1;  # read *all* records in winequality-white-*.csv
opts$RF.samp=5000;
opts$READ.TstFn = readTstWine
opts$VERBOSE=1;
                    # read 'new' data (both from opts$filename and opts$filetest):
dataObj <- tdmReadAndSplit(opts,envT$tdm);
envT <- tdmEnvTSetOpts(envT,opts);
envT$tdm$nrun=2;    # =0: no unbiasedRun, 
                    # >0: perform unbiasedRun with opts$NRUN=envT$tdm$nrun
envT <-  tdmEnvTReport(envT,1);
if (!is.null(envT$theFinals)) print(envT$theFinals);




