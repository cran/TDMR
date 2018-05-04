## path should point to a dir with subdir data/ and main_*.r file:
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

envT<- tdmEnvTUpdate(envT,tdm); # update the re-loaded envT$tdm with new elements given in tdm

opts <- tdmEnvTGetOpts(envT);
opts$READ.NROW=-1;          # read *all* records in winequality-white-*.csv
opts$RF.samp=5000;
opts$READ.TstFn = readTstWine
opts$VERBOSE=1;
# read 'new' data (both from opts$filename and opts$filetest):
dataObj <- tdmReadAndSplit(opts,envT$tdm);
envT <- tdmEnvTSetOpts(envT,opts);
envT$tdm$nrun=1;       # =0: no unbiasedRun, >0: perform unbiasedRun with opts$NRUN=envT$tdm$nrun
envT <- tdmEnvTReport(envT,1);
if (!is.null(envT$theFinals)) print(envT$theFinals);

## predict(envT,newdata) below works only if tdm$U.saveModel==TRUE 
## (which leads to a 2 - 2.5 MB .Rdata file tdm$filenameEnvT)
#envT<- tdmEnvTLoad(tdm$filenameEnvT,path);    # loads envT
newdata=dsetTest(dataObj);
z=predict(envT,newdata);     # envT is of class TDMenvir
print(length(z));

## The following section proves and shows that newdata predicted by tdmTuneIt gives 
## exactly the same results as newdata predicted by predict. 
## (this is only true, if tdmTuneIt(...) is run before, it performes an unbiasedRun
## with the training data in dataObj and stores the resulting model in envT$result$lastRes$lastModel.
## This model is used by predict.TDMenvir.)
winePred <- data.frame(actual=newdata$quality,
                       predTune=envT$result$predictTest,
                       predPred=z)
errorTune <- length(which(winePred$actual==winePred$predTune))/length(z)
errorPred <- length(which(winePred$actual==winePred$predPred))/length(z)
cat(sprintf("errorTune=%f, errorPred=%f\n",errorTune,errorPred))
print(all(winePred$predTune==winePred$predPred))
#View(winePred)




