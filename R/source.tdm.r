######################################################################################
# Collects garbage
# calls the R garbage collection function gc()
#
collectGarbage <- function()
{
  	while (gc()[2,4] != gc()[2,4]){}
}

######################################################################################
#
# source.tdm: function to load TDM, phase 1-3 and to load SPOT
#
source.tdm <- function(tdmPath, tdmParallelCPUs=1,theSpotPath=NA) {

  createSourcePath <- function(sourceFileName){
    normalizePath(paste(tdmPath,sourceFileName, sep="/"));
  }

  if (!is.na(theSpotPath)) {
      if (theSpotPath=="USE.SOURCE") {
        # load SPOT from source files at a specified location. 
        # (this is useful for debugging SPOT code or for testing new developper versions)
        # --- may need adjustment to your specific SPOT source directory location ---
        if (.Platform$OS.type=="unix") {
          theSpotPath <- '~/Desktop/FH_Koeln/svnspot/trunk/SPOT/R/';
          if (Sys.info()["user"]=="konen") theSpotPath <- '~/svnspot/trunk/SPOT/R/';
        }
        if (.Platform$OS.type=="windows") {
          theSpotPath <- 'C:/WUTemp/FH-MassenDaten/TBB.SPO/trunk/SPOT/R/';
          if (Sys.info()["user"]=="wolfgang")  theSpotPath <- 'C:/WUTemp/FH-MassenDaten/TBB.SPO/trunk/SPOT/R/';
        }
      }
      #
      # if 'theSpotPath' is a string different from "USE.SOURCE", then we try 
      # to read the SPOT sources from this path 'theSpotPath'
      #
  }

  tdmParallel = (tdmParallelCPUs>1);
  if(!tdmParallel){
    require("randomForest");
    require("e1071");         # svm(), Naive Bayes
    require("matlab");        # repmat() etc., for tdmParaBootstrap.r 
    if (is.na(theSpotPath)) {
        require("SPOT");     # load SPOT from the installed library (package version)
    } else {
        oldwd=getwd(); setwd(theSpotPath);
        for (f in dir())   source(f);
        setwd(oldwd);
    }
    
    source(createSourcePath("phase1/tdmOptsDefaults.r"))
    source(createSourcePath("phase1/tdmPreprocUtils.r"))
    source(createSourcePath("phase1/tdmGeneralUtils.r"))
    source(createSourcePath("phase1/tdmGraphicUtils.r"))
    source(createSourcePath("phase1/tdmModelingUtils.r"))    
    source(createSourcePath("phase1/classify/tdmClassify.r"))
    source(createSourcePath("phase1/classify/tdmMetacostRf.r"))
    source(createSourcePath("phase1/classify/tdmParaBootstrap.r"))
    source(createSourcePath("phase1/regress/tdmEmbedDataFrame.r"))
    source(createSourcePath("phase1/regress/tdmRegress.r"))

    source(createSourcePath("phase2/tdmMapDesign.r"))
    source(createSourcePath("phase2/makeTdmStartOther.r"))
    source(createSourcePath("phase2/makeTdmRandomSeed.r"))
    source(createSourcePath("phase2/tdmStartSpot.r"))
    source(createSourcePath("phase2/tdmCompleteEval.r"))
    source(createSourcePath("phase2/tdmDispatchTuner.r"))
    source(createSourcePath("phase2/tdmGetObj.r"))
    source(createSourcePath("phase2/tdmDefaultsFill.r"))
    source(createSourcePath("phase2/unbiasedBestRun_C.r"))
    source(createSourcePath("phase2/unbiasedBestRun_O.r"))
    source(createSourcePath("phase2/unbiasedBestRun_R.r"))
    
    #source(paste(tdmPath,"source.tdm.1.r",sep="/")); source.tdm.1(tdmPath, tdmParallel);
    #source(paste(tdmPath,"source.tdm.2.r",sep="/")); source.tdm.2(tdmPath, tdmParallel);
    #source(paste(tdmPath,"sourceSPOT.R",sep="/")); sourceSPOT(theSpotPath);
  }
  else   # i.e. if (tdmParallel)
  {
    sfLibrary("randomForest",character.only=TRUE);
    sfLibrary("e1071",character.only=TRUE);        # svm(), Naive Bayes
    sfLibrary("matlab",character.only=TRUE);       # repmat() etc., for tdmParaBootstrap.r 
    if (is.na(theSpotPath)) {
        sfLibrary("SPOT",character.only=TRUE);     # load SPOT from the installed library (package version)
    } else {
        oldwd=getwd(); setwd(theSpotPath);
        for (f in dir())   sfSource(f);
        setwd(oldwd);
    }
    sfSource(createSourcePath("phase1/tdmOptsDefaults.r"))
    sfSource(createSourcePath("phase1/tdmPreprocUtils.r"))
    sfSource(createSourcePath("phase1/tdmGeneralUtils.r"))
    sfSource(createSourcePath("phase1/tdmGraphicUtils.r"))
    sfSource(createSourcePath("phase1/tdmModelingUtils.r"))    
    sfSource(createSourcePath("phase1/classify/tdmClassify.r"))
    sfSource(createSourcePath("phase1/classify/tdmMetacostRf.r"))
    sfSource(createSourcePath("phase1/classify/tdmParaBootstrap.r"))
    sfSource(createSourcePath("phase1/regress/tdmEmbedDataFrame.r"))
    sfSource(createSourcePath("phase1/regress/tdmRegress.r"))

    sfSource(createSourcePath("phase2/tdmMapDesign.r"))
    sfSource(createSourcePath("phase2/makeTdmStartOther.r"))
    sfSource(createSourcePath("phase2/makeTdmRandomSeed.r"))
    sfSource(createSourcePath("phase2/tdmStartSpot.r"))
    sfSource(createSourcePath("phase2/tdmCompleteEval.r"))
    sfSource(createSourcePath("phase2/tdmDispatchTuner.r"))
    sfSource(createSourcePath("phase2/tdmGetObj.r"))
    sfSource(createSourcePath("phase2/unbiasedBestRun_C.r"))
    sfSource(createSourcePath("phase2/unbiasedBestRun_O.r"))
    sfSource(createSourcePath("phase2/unbiasedBestRun_R.r"))
    
    #sfSource(paste(tdmPath,"source.tdm.1.r",sep="/")); source.tdm.1(tdmPath, tdmParallel);
    #sfSource(paste(tdmPath,"source.tdm.2.r",sep="/")); source.tdm.2(tdmPath, tdmParallel);
    #sfSource(paste(tdmPath,"sourceSPOT.R",sep="/")); sourceSPOT(theSpotPath);
  }
    
  collectGarbage()
}


