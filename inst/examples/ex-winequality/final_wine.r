## working directory is this dir (dir with .apd, .conf and main_*.r file):

tdm=list(mainFile="main_wine.r"
         ,runList="wine_01.conf"
         ,umode="SP_T"  
         ,TST.valiFrac=0
         ,U.saveModel=F
         ,optsVerbosity=1
         ,nrun=2
);
source(tdm$mainFile);  

#
# re-use prior tuning result (spotStep="rep"); do only spot report and unbiased eval on 
# best tuning result. But do so by training a model on 80% of all 4898 records.
# 
tdm <- tdmDefaultsFill(tdm)
load(tdm$filenameEnvT);     # envT
opts <- tdmEnvTGetOpts(envT,1);
opts$READ.NROW=-1;          # read all 4898 records of winequality-white.csv
opts$RF.samp=5000;
envT$tdm$optsVerbosity <- 1;
envT$tdm$U.saveModel <- tdm$U.saveModel;
envT$tdm$TST.valiFrac <- tdm$TST.valiFrac;
envT$tdm$umode <- tdm$umode;
dataObj <- tdmSplitTestData(opts,envT$tdm);
envT <- tdmEnvTSetOpts(envT,opts);
envT <- tdmTuneIt(envT,"rep",dataObj);

## predict(envT,newdata) below works only if tdm$U.saveModel==TRUE 
## (which leads to a 2.4MB .Rdata file tdm$filenameEnvT)
#load(tdm$filenameEnvT);
#newdata=dataObj$dset;
#z=predict(envT,newdata);          # envT is of class TDMenvir
#print(length(z));




