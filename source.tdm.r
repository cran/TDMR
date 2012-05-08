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
# source.tdm: function to load TDMR from source files and to load SPOT and rSFA
#
source.tdm <- function(tdmPath, tdmParallelCPUs=1,theSpotPath=NA,theRsfaPath=NA) {

  createSourcePath <- function(sourceFileName){
    normalizePath(paste(tdmPath,"R",sourceFileName, sep="/"));
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
          theSpotPath <- 'C:/WUTemp/FH-MassenDaten/svnspot/trunk/SPOT/R/';
          if (Sys.info()["user"]=="wolfgang")  theSpotPath <- 'C:/WUTemp/FH-MassenDaten/svnspot/trunk/SPOT/R/';
        }
      }
      # if 'theSpotPath' is a string different from "USE.SOURCE", then we try 
      # to read the SPOT sources from this path 'theSpotPath'
  }
      
  if (!is.na(theRsfaPath)) {
      if (theRsfaPath=="USE.SOURCE") {
        # load rSFA from source files at a specified location. 
        # (this is useful for debugging rSFA code or for testing new developper versions)
        # --- may need adjustment to your specific rSFA source directory location ---
        if (.Platform$OS.type=="unix") {
          theRsfaPath <- '~/Desktop/FH_Koeln/svnspot/trunk/SPOT/R/';
          if (Sys.info()["user"]=="konen") theRsfaPath <- '~/svnspot/trunk/SPOT/R/';
        }
        if (.Platform$OS.type=="windows") {
          theRsfaPath <- 'C:/WUTemp/FH-MassenDaten/fiwa_soma/trunk/doc/CaseStudies.d/201112.d/tdmExtensions.d/CodeR.d/rSFA/R';
        }
      }
      # if 'theRsfaPath' is a string different from "USE.SOURCE", then we try 
      # to read the rSFA sources from this path 'theRsfaPath'   
  }

  tdmParallel = (tdmParallelCPUs>1);
  if(!tdmParallel){
    require("randomForest");
    require("e1071");        # svm(), Naive Bayes
    #require("matlab");      # repmat() etc., for tdmParaBootstrap.r  - now deprecated 12/2011
    if (is.na(theSpotPath)) {
        require("SPOT");     # load SPOT from the installed library (package version)
    } else {
        oldwd=getwd(); setwd(theSpotPath);
        for (f in dir())   source(f);
        setwd(oldwd);
    }
    
    if (is.na(theRsfaPath)) {
        require("rSFA");     # load rSFA from the installed library (package version)
    } else {
        oldwd=getwd(); setwd(theRsfaPath);
        for (f in dir())   source(f);
        setwd(oldwd);
    }
    
    source(createSourcePath("makeTdmStartOther.r"))
    source(createSourcePath("makeTdmRandomSeed.r"))
    source(createSourcePath("printTDMclassifier.r"))
    source(createSourcePath("printTDMregressor.r"))
    source(createSourcePath("tdmClassify.r"))
    source(createSourcePath("tdmClassifyLoop.r"))
    source(createSourcePath("tdmEmbedDataFrame.r"))
    source(createSourcePath("tdmGeneralUtils.r"))
    source(createSourcePath("tdmGraphicUtils.r"))
    source(createSourcePath("tdmMetacostRf.r"))
    source(createSourcePath("tdmModelingUtils.r"))    
    source(createSourcePath("tdmOptsDefaults.r"))
    source(createSourcePath("tdmParaBootstrap.r"))
    source(createSourcePath("tdmPreprocUtils.r"))
    source(createSourcePath("tdmReadData.r"))
    source(createSourcePath("tdmRegress.r"))
    source(createSourcePath("tdmRegressLoop.r"))

    source(createSourcePath("tdmCompleteEval.r"))
    source(createSourcePath("tdmDefaultsFill.r"))
    source(createSourcePath("tdmDispatchTuner.r"))
    source(createSourcePath("tdmGetObj.r"))
    source(createSourcePath("tdmMapDesign.r"))
    source(createSourcePath("tdmPlotResMeta.r"))
    source(createSourcePath("tdmROCR.r"))
    source(createSourcePath("tdmSplitTestData.r"))
    source(createSourcePath("tdmStartSpot.r"))
    source(createSourcePath("unbiasedRun.r"))
    source(createSourcePath("unbiasedBestRun_O.r"))
    
  }
  else   # i.e. if (tdmParallel)
  {
    sfLibrary("randomForest",character.only=TRUE);
    sfLibrary("e1071",character.only=TRUE);        # svm(), Naive Bayes
    #sfLibrary("matlab",character.only=TRUE);      # repmat() etc., for tdmParaBootstrap.r - now deprecated 12/2011
    if (is.na(theSpotPath)) {
        sfLibrary("SPOT",character.only=TRUE);     # load SPOT from the installed library (package version)
    } else {
        oldwd=getwd(); setwd(theSpotPath);
        for (f in dir()) sfSource(f);
        setwd(oldwd);
    }
    if (is.na(theRsfaPath)) {
        sfLibrary("rSFA",character.only=TRUE);     # load rSFA from the installed library (package version)
    } else {
        oldwd=getwd(); setwd(theRsfaPath);
        for (f in dir())   sfSource(f);
        setwd(oldwd);
    }
    sfSource(createSourcePath("makeTdmStartOther.r"))
    sfSource(createSourcePath("makeTdmRandomSeed.r"))
    sfSource(createSourcePath("printTDMclassifier.r"))
    sfSource(createSourcePath("printTDMregressor.r"))
    sfSource(createSourcePath("tdmClassify.r"))
    sfSource(createSourcePath("tdmClassifyLoop.r"))
    sfSource(createSourcePath("tdmEmbedDataFrame.r"))
    sfSource(createSourcePath("tdmGeneralUtils.r"))
    sfSource(createSourcePath("tdmGraphicUtils.r"))
    sfSource(createSourcePath("tdmMetacostRf.r"))
    sfSource(createSourcePath("tdmModelingUtils.r"))    
    sfSource(createSourcePath("tdmOptsDefaults.r"))
    sfSource(createSourcePath("tdmParaBootstrap.r"))
    sfSource(createSourcePath("tdmPreprocUtils.r"))
    sfSource(createSourcePath("tdmReadData.r"))
    sfSource(createSourcePath("tdmRegress.r"))
    sfSource(createSourcePath("tdmRegressLoop.r"))

    sfSource(createSourcePath("tdmCompleteEval.r"))
    sfSource(createSourcePath("tdmDefaultsFill.r"))
    sfSource(createSourcePath("tdmDispatchTuner.r"))
    sfSource(createSourcePath("tdmGetObj.r"))
    sfSource(createSourcePath("tdmMapDesign.r"))
    sfSource(createSourcePath("tdmPlotResMeta.r"))
    sfSource(createSourcePath("tdmROCR.r"))
    sfSource(createSourcePath("tdmSplitTestData.r"))
    sfSource(createSourcePath("tdmStartSpot.r"))
    sfSource(createSourcePath("unbiasedRun.r"))
    sfSource(createSourcePath("unbiasedBestRun_O.r"))
    
  }
    
  collectGarbage()
}


