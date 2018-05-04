######################################################################################
# tdmEnvTMakeNew:
#
#' Construct a new environment envT of class \code{\link{TDMenvir}}.
#'
#' Given the general TDMR settings in \code{tdm}, construct an appropriate environment \code{envT}.
#' This is needed as input for \code{\link{tdmBigLoop}}.
#'
#' @param tdm   a list with general settings for TDMR, see \code{\link{tdmDefaultsFill}}  
#' @param sCList [defaultSC()] a list of list with controls for SPOT or other tuners (one list
#'        for each element in \code{tdm$runList})  
#'
#' @return Environment \code{envT},  an object of class \code{\link{TDMenvir}},  containing (among others) the elements
#'      \item{\code{runList}}{ \code{=tdm$runList}  }
#      \item{\code{spotList}}{ \code{=tdm$spotList}  }
#'      \item{\code{tdm}}{ \code{=\link{tdmDefaultsFill}(tdm)}  }
#'      \item{\code{getBst}}{ accessor function(confFile,nExp,theTuner) into \code{envT$bstGrid}   }
#'      \item{\code{getRes}}{ accessor function(confFile,nExp,theTuner) into \code{envT$resGrid}   }
#'      \item{\code{sCList}}{ list of spotConfig-objects, as many as \code{envT$runList} has elements. Each spotConfig object 
#'          \code{sCList[[k]]} contains a list \code{opts} as element for the machine learning part.  }
#'
#' @seealso   \code{\link{tdmBigLoop}}
#' @author Wolfgang Konen (\email{wolfgang.konen@@th-koeln.de}), THK, Patrick Koch
#' @aliases TDMenvir 
#' @export
######################################################################################
tdmEnvTMakeNew <- function(tdm=NULL, sCList=defaultSCList()) {
  #tdm$optsPath=tdm$path
  ######################################################################################
  # helper fcts for tdmEnvTMakeNew:
  ######################################################################################
  # checkRoiParams: 
  #     Check whether each .roi file in envT$runList contains the same design parameters, otherwise set wP=FALSE and issue a warning
  checkRoiParams <- function(wP,confFile,sC,envT) {
  #firstRoiNames <<- "test"
        if (wP) {
          if (confFile==envT$runList[1]) {
            firstRoiNames <<- rownames(sC$alg.roi);
          } else {
            roiNames = rownames(sC$alg.roi);
            if (length(roiNames)!=length(firstRoiNames)) {
              wP=FALSE;
            } else {
              if (any(roiNames!=firstRoiNames)) wP=FALSE;
            }
          }
          if (wP==FALSE) warning("The design parameters in different ROI files are different --> TDMR sets tdm$withParams to FALSE (no param columns in envT$theFinals)");
        }
        wP;
  }
  #
  # function addSRF adds to opts the element opts$srfFile and reads in case opts$SRF.calc==FALSE from this file the
  # element opts$srf (a list with as many data frames as there are response variables in the task of confFile)
  addSRF <- function(confFile,opts) {
      dir.output <- paste(dirname(opts$dir.output),basename(opts$dir.output),sep="/")  # remove trailing "/", if it exists
      if (!file.exists(dir.output)) {
        success = dir.create(dir.output);
        if (!success) stop(sprintf("Could not create dir.output=%s",dir.output));
      }
      opts$srfFile <- paste(paste(dir.output,confFile,sep="/"),"SRF.Rdata",sep=".")
      if (opts$SRF.calc==FALSE) {
        cat("Loading sorted RF importance from file",opts$srfFile,"...\n")
        if (!file.exists(opts$srfFile)) stop(sprintf("SRF.file=%s does not exist",opts$srfFile));
        srf=NULL;                 # to make package-build-checker happy
        load(file=opts$srfFile);  # load srf
        opts$srf=srf;
      }
      opts;
  }
  #
  # function runListFromSC extracts from sCList a vector of configuration names:
  # either elements sCName or generic c("C01", "C02", ...)
  runListFromSC <- function(sCList,tdm) {
    if (!is.null(tdm$runList)) return(tdm$runList);
    
    runList <- sprintf("C%02d.conf",seq(length(sCList)));
    for (i in 1:length(sCList)) 
      if (!is.null(sCList[[i]]$sCName)) runList[i] <- sCList[[i]]$sCName;
      
    runList;
  }
  ######################################################################################
  # end helper fcts for tdmEnvTMakeNew
  ######################################################################################
  
  envT <- new.env();
  envT$bstGrid <- list();
  envT$resGrid <- list();
  envT$roiGrid <- list();
  envT$sCList <- list();
  envT$theFinals <- NULL;
  tdm$runList <- runListFromSC(sCList,tdm);     # NEW in TDMR2.0
  envT$tdm <- tdm <- tdmDefaultsFill(tdm);
  print(envT$tdm$path);
  envT$runList <- tdm$runList;
  envT$spotList <- tdm$spotList; # deprecated
  envT$wP <- ifelse(is.null(tdm$withParams), length(tdm$runList)==1, tdm$withParams)
  if (is.null(tdm$runList)) stop("tdm$runList is NULL");
  if (is.null(tdm$optsPath)) tdm$optsPath="./"
  if (length(unique(tdm$runList)) != length(tdm$runList)) {
    stop(paste("There are duplicates in tdm$runList. Please remove them:\n   ",paste(tdm$runList,collapse=" ")));
#    print(tdm$runList);
#    stop(paste("There are duplicates in tdm$runList. Please remove them."));
  }

  envT <- tdmEnvTAddGetters(envT);

  firstRoiNames <- NULL;    # private storage for checkRoiParams

  #
  # check and extend settings in sCList **before** branching into tdmBigLoop 
  #
  k=0;
  for (confFile in envT$runList) {
    k=k+1;
    #--- not necessary anymore in TDMR2.0
    #lastChar=substr(tdm$path,nchar(tdm$path),nchar(tdm$path))
    #sepChar=ifelse(lastChar=="/","","/")
    #pathConfFile = paste(tdm$path,confFile,sep=sepChar);
    #if (!file.exists(pathConfFile)) stop(sprintf("Could not find confFile=%s (current dir=%s)",pathConfFile,getwd()));
    #---
    envT$sCList[[k]] <- sCList[[k]]; # spotGetOptions(srcPath=tdm$theSpotPath,pathConfFile);
    sC <- envT$sCList[[k]]; print(sC$seedSPOT);
    envT$wP <- checkRoiParams(envT$wP,confFile,sC,envT);
    opts <- sCList[[k]]$opts;
    if (class(opts)[1]!="tdmOpts") 
      warning("Object opts is not of class tdmOpts. Consider constructing opts with tdmOptsDefaultsSet().");
    if (!is.null(tdm$TST.trnFrac)) opts$TST.trnFrac=tdm$TST.trnFrac;
    if (!is.null(tdm$TST.valiFrac)) opts$TST.valiFrac=tdm$TST.valiFrac;
    if (!is.null(tdm$READ.target)) opts$READ.target=tdm$READ.target;
    if (!is.null(tdm$oFileMode)) opts$fileMode=tdm$oFileMode;
 
    opts=tdmOptsDefaultsSet(opts,path=tdm$path);
    opts=addSRF(confFile,opts);      # add opts$srfFile and add opts$srf from file in case opts$SRF.calc==FALSE
    checkOpts(opts);

    if (tdm$parallelCPUs>1 & opts$fileMode==TRUE)
      warning("With tdm$parallelCPUs>1 the setting opts$fileMode=TRUE might be problematic. Consider to set tdm$oFileMode=FALSE");
    
    if (tdm$umode[1]=="TST" & opts$TST.kind=="col" & !(opts$MOD.method %in% c("RF","MC.RF")))
      warning(sprintf("%s: Do you really want tdm$umode=='TST' and opts$TST.kind=='col' when opts$MOD.method is not based on RF? %s",
                      confFile,"You will have no validation data!"));
 	  envT$sCList[[k]]$opts=opts;
  }
  class(envT) <- c("TDMenvir","environment");
  
  envT;
}

# function checkOpts checks whether there are any new variable names in list opts (e.g. due to misspelling in opts-construction)
# and if so, issues a NOTE and a warning message.
checkOpts <- function(opts) {
  availNames = c("APPLY_TIME","CLS.CLASSWT","CLS.cutoff","CLS.gainmat","data.title","dir.data","dir.output","dir.Rdata"
                ,"dir.txt","DO.GRAPHICS","DO.POSTPROC","EVALFILE","fct.postproc","fileMode","filename","filesuffix","filetest"
                ,"GD.CLOSE","GD.DEVICE","GD.PNGDIR","GD.RESTART","gr.log","LOGFILE","logFile","MOD.method","MOD.SEED","ncopies","NRUN","OCUT","PDFFILE"
                ,"PRE.allNonVali","PRE.knum","PRE.MaxLevel","PRE.PCA","PRE.PCA.npc","PRE.PCA.REPLACE","PRE.SFA","PRE.SFA.doPB"
                ,"PRE.SFA.fctPB","PRE.SFA.npc","PRE.SFA.ODIM","PRE.SFA.PPRANGE","PRE.SFA.REPLACE","PRE.Xpgroup","READ.INI"
                ,"READ.NROW","READ.TXT","READ.TrnFn","READ.TstFn","rep","RF.mtry","RF.mtry","RF.nodesize","RF.ntree","RF.OOB","RF.p.all","RF.samp"
                ,"rgain.string","rgain.type","srf","SRF.calc","SRF.cutoff","SRF.kind","SRF.maxS","SRF.method","SRF.minlsi","SRF.ndrop"
                ,"SRF.nkeep","SRF.ntree","SRF.samp","SRF.scale","SRF.verbose","SRF.XPerc","srfFile","SVM.coef0","SVM.cost","SVM.degree","SVM.epsilon"
                ,"SVM.gamma","SVM.kernel","SVM.tolerance","ADA.coeflearn","ADA.mfinal","ADA.rpart.minsplit","test2.show","test2.string"
                ,"TST.COL","TST.kind","TST.NFOLD","TST.SEED","TST.trnFrac","TST.valiFrac","VERBOSE","path");
  newNames = setdiff(names(opts),availNames);
  if (length(newNames)>0) {
    if (length(newNames)==1) cat("NOTE: A new variable has been defined for list opts: ",newNames,".\n");
    if (length(newNames)>1) cat("NOTE: New variables have been defined for list opts: ",paste(newNames,collapse=", "),".\n");
    warning(paste("NOTE: New variables have been defined for list opts: ",paste(newNames,collapse=", "),"."))
  }
}

######################################################################################
#  tdmEnvTAddGetters:
#'
#' Add getter functions getBst and getRes to environment envT
#'
#' @param   envT       the TDMR environment
#' @return  the augmented \code{envT}
#' @export
######################################################################################
tdmEnvTAddGetters <- function(envT) {  
  # envT$getInd: private helper fct for envT$getBst and envT$getRes
  envT$getInd <- function(envT,confFile,nExp,theTuner) {
    indTuner = which(envT$tdm$tuneMethod==theTuner);
    if (length(indTuner)==0) stop(paste("Could not find tuner ",theTuner,"in envT$tdm$tuneMethod"));
    nConf = which(envT$runList==confFile);
    if (length(nConf)==0) stop(paste("Could not find conf file ",confFile,"in envT$runList"));
    if (nExp<1 | nExp>envT$tdm$nExperim) stop(paste("nExp is not in range {1,...,",envT$tdm$nExperim,"}",sep=""));
    ind = indTuner + length(envT$tdm$tuneMethod)*((nExp-1) + envT$tdm$nExperim*(nConf-1));
  }
  # envT$getBst: return from the linear list envT$bstGrid the data frame bst for the triple {confFile,nExp,theTuner}
  envT$getBst <- function(envT,confFile,nExp,theTuner) {
    ind = envT$getInd(envT,confFile,nExp,theTuner);
    lgrid = length(envT$bstGrid);
    if (ind<1 | ind>lgrid) stop(sprintf("Subscript %d is out of bounds for envT$bstGrid (length is %d)",ind,lgrid));
    envT$bstGrid[[ind]];
  }
  # envT$getRes: return from the linear list envT$resGrid the data frame res for the triple {confFile,nExp,theTuner}
  envT$getRes <- function(envT,confFile,nExp,theTuner) {
    ind = envT$getInd(envT,confFile,nExp,theTuner);
    lgrid = length(envT$resGrid);
    if (ind<1 | ind>lgrid) stop(sprintf("Subscript %d is out of bounds for envT$resGrid (length is %d)",ind,lgrid));
    envT$resGrid[[ind]];
  }
  class(envT) <- c("TDMenvir","environment");
  envT;
}


######################################################################################
#  tdmEnvTLoad:
#' Load an \code{envT}-type environment from file \code{fileRData}. 
#'
#' The loaded envT is augmented with getter functions, see \code{\link{tdmEnvTAddGetters}}.
#'
#' @param   fileRData   string with filename to load. 
#' @param   path   [NULL] dir where to search \code{fileRData}. If NULL, use current dir.
#' @return  envT
#' @export
######################################################################################
tdmEnvTLoad <- function(fileRData, path=NULL) {
  pathFile = fileRData;
  if (!is.null(path)) pathFile = paste(path,fileRData,sep="/");
  load(pathFile); # loads envT
  envT <- tdmEnvTAddGetters(envT);
  class(envT) <- c("TDMenvir","environment");
  envT;
}

######################################################################################
#  tdmEnvTUpdate:
#' Update \code{envT$tdm}
#'
#' Update \code{envT$tdm} with the non-NULL elements of \code{tdm}
#' 
#' @param   envT environment TDMR
#' @param   tdm  list for TDMR, see \code{\link{tdmDefaultsFill}}
#' @return  envT
#' @export
######################################################################################
tdmEnvTUpdate <- function(envT,tdm) {
  if(methods::hasArg(tdm)){
    matching<-intersect(names(tdm),names(envT$tdm))
    envT$tdm[matching] <- tdm[matching]
    
    #notMatching <- setdiff(names(tdm),
    #                       names(envT$tdm))
    #if(length(notMatching)!=0) warning(paste("The following arguments in tdm are ignored: ", notMatching))
    #### 
    #### We exclude this test here, because there are members in envT$tdm with default value NULL.  
    #### Each of them would trigger a warning if it were defined in argument tdm.
  }
  
  envT;
}

######################################################################################
#  tdmEnvTGetOpts:
#' Return list opts from the \code{k}-th element of \code{envT$sCList}
#'
#' @param   envT environment TDMR
#' @param   k    [1] index 1,...,length(envT$runList)
#' @return  opts
#' @export
######################################################################################
tdmEnvTGetOpts <- function(envT,k=1) {
  envT$sCList[[k]]$opts;
}

######################################################################################
#  tdmEnvTSetOpts:
#' Set list opts for the \code{k}-th element of \code{envT$sCList}
#'
#' @param   envT environment TDMR
#' @param   opts list of options
#' @param   k    [1] index 1,...,length(envT$runList)
#' @return  envT
#' @export
######################################################################################
tdmEnvTSetOpts <- function(envT,opts,k=1) {
  envT$sCList[[k]]$opts <- opts;
  envT;
}

######################################################################################
#  tdmEnvTAddBstRes:
#' Add BST and RES data frames to an existing \code{envT} environment.
#'
#' Load an \code{envT}-type environment from file \code{fileRData}. Its elements 
#' \code{bst}, \code{bstGrid} \code{res}, and \code{resGrid} 
#' overwrite the elements in \code{envT} passed in as argument. 
#'
#' @param   envT       the TDMR environment
#' @param   fileRData   string with filename to load. This file is searched in \code{envT$tdm$path}.
#' @return  the augmented \code{envT}
#' @export
######################################################################################
tdmEnvTAddBstRes <- function(envT,fileRData) {
  loadEnvT <- function(fileRData) {
    load(paste(envT$tdm$path,fileRData,sep="")); # loads envT
    envT;
  }
  envT2 = loadEnvT(fileRData);
  envT$bst = envT2$bst;
  envT$res = envT2$res;
  envT$bstGrid = envT2$bstGrid;
  envT$resGrid = envT2$resGrid;
  if (!is.null(envT2$roiGrid)) envT$roiGrid = envT2$roiGrid;
  if (!is.null(envT2$runList)) envT$runList = envT2$runList;
  if (!is.null(envT2$nExperim)) envT$nExperim = envT2$nExperim;
  if (!is.null(envT2$tuneMethod)) envT$tuneMethod = envT2$tuneMethod;
  class(envT) <- c("TDMenvir","environment");
  envT;
}

######################################################################################
#  tdmEnvTReport:
#' Make a report plot based on \code{envT}
#'
#' Given the results from a prior tuning run in \code{envT}, make a sensitivity plot
#' for this run. \cr
#' If \code{envT$tdm$nrun > 0} then make additionally with the best-performing parameters from 
#' the tuning run a new unbiased run on the test data.
#'
#' @param   envT  results from a prior tuning run. 
#' @param   ind   an integer from \code{1:length(envT$bstGrid)}: Take the tuning run with index \code{ind}.
#' @return  \code{envT}, with data frame \code{finals} added, if  \code{envT$tdm$nrun > 0}.
#' @examples            
#'    ## The best results are read from demo02sonar/demoSonar.RData relative to the TDMR 
#'    ## package directory.
#'    path = paste(find.package("TDMR"), "demo02sonar",sep="/");
#'    envT = tdmEnvTLoad("demoSonar.RData",path);    # loads envT
#'    source(paste(path,"main_sonar.r",sep="/"));
#'    envT$tdm$nrun=0;       # =0: don't, >0: do unbiasedRun with opts$NRUN=envT$tdm$nrun
#'    envT$sCList[[1]]$opts$VERBOSE=1;
#'    envT <- tdmEnvTReport(envT,1);
#'    if (!is.null(envT$theFinals)) print(envT$theFinals);
#'
#' @seealso  \code{\link{tdmEnvTReportSens}} 
#' @export
##############################################################################################
#--- currently only sensitivity-based report, but other reports are later possible as well ---
tdmEnvTReport <- function(envT,ind) {
  tdm = envT$tdm;
  if (is.null(tdm$runList)) {
    if (is.null(envT$runList)) stop("Both envT$runList and envT$tdm$runList are NULL");
    tdm$runList = envT$runList;
  }
  nTuner <- length(tdm$tuneMethod);
  nRunList <- length(tdm$runList);
	tuneVec <- rep(tdm$tuneMethod,tdm$nExperim*nRunList);
	expeVec <- rep(sort(rep(1:tdm$nExperim,nTuner)),nRunList);
	confVec <- sort(rep(tdm$runList,tdm$nExperim*nTuner));
  if (ind>length(envT$bstGrid)) stop(sprintf("ind=%d is bigger than length(envT$bstGrid)=%d",ind,length(envT$bstGrid)));
  theTuner = tuneVec[ind];
  nExp = expeVec[ind];
  confFile = confVec[ind];
  #nConf = which(confFile==envT$runList);
  nConf = which(confFile==tdm$runList);
  print(nConf);
  if (length(nConf)==0) 
    stop ("Something wrong with confFile and tdm$runList");
  #print(c(ind,confFile,nExp,theTuner));
  
  #correctEnvTDir(envT,tdm);                # correct direcotories in envT$sCList

  envT$spotConfig <- envT$sCList[[nConf]];   
  envT$bst = envT$bstGrid[[ind]];
  envT$res = envT$resGrid[[ind]];
  #
  # Fix for upgading older BST data frames to the new SPOT version (V1.0.2662): 
  # A BST data frame now has to have a column STEP, otherwise a crash in spotWriteBest (spotHelpFunctions.R) will occur.  
  if (!any(names(envT$bst)=="STEP")) {
    cat("NOTE: Since envT$bst has no column STEP, we add for SPOT a dummy column STEP containing the numbers 1 :",nrow(envT$bst),"\n");
    envT$bst <- cbind(envT$bst,STEP=(1:nrow(envT$bst)));
  }
  envT$spotConfig$alg.roi = envT$roiGrid[[ind]];
  
  envT$spotConfig$spot.fileMode=FALSE;    # call SPOT with spot.fileMode=FALSE (take everything from  envT$spotConfig)
 
  # ptm <- proc.time();
  
  # this is needed for tdmEnvTReportSens:
  envT$spotConfig$alg.currentResult <- envT$res;
  envT$spotConfig$alg.currentBest <- envT$bst;	
  
  writeLines(sprintf("\n*** Starting SENS REPORT for tuner %s, on task %s ***",theTuner,confFile),con=stderr());
  
  tdmEnvTReportSens(envT$spotConfig);
  
  # if (is.null(tdm$timeMode)) stop("tdm$timeMode is not set (NULL). Consider 'tdm <- tdmDefaultsFill(tdm)' to set all defaults");
  # envT$time.TRN=(proc.time()-ptm)[tdm$timeMode]; opts=list(); opts$VERBOSE=1;  
  # cat1(opts,paste(time.txt[tdm$timeMode], "time for tuning with tdmDispatchTuner:",envT$time.TRN,"sec\n"));     
  opts <- envT$spotConfig$opts;
  
  ptm <- proc.time();
  envT$finals <- NULL;
  if (tdm$nrun>0) {
      #
      # read the data and split them into test data and train/vali data
      dataObj <- tdmReadAndSplit(opts,tdm,nExp);
      if (!is.null(dataObj)) opts$TST.COL = dataObj$TST.COL;    # this column has to be subtracted in main_* from the input variables

      if (!is.null(tdm$mainFile)) {
        pathFile <- paste(opts$path,tdm$mainFile,sep="/")
        if (!file.exists(pathFile)) stop(sprintf("Could not find tdm$mainFile=%s (path=%s)",tdm$mainFile,opts$path));
 	      source(pathFile);
      }
    
      for (umode in tdm$umode) {
        writeLines(sprintf("\n*** Starting %s for %s with umode= %s ***",tdm$unbiasedFunc,confFile,tdm$umode),con=stderr());
        # cmd=paste("envT <-",tdm$unbiasedFunc,"(confFile,envT,dataObj,umode=umode,withParams=envT$wP,tdm=tdm)",sep="");
        # eval(parse(text=cmd));
        envT <- eval(call(tdm$unbiasedFunc,confFile,envT,dataObj,umode=umode,withParams=envT$wP,tdm=tdm))
      }
      time.txt = c("Proc", "System", "Elapsed");
      time.TST=(proc.time()-ptm)[tdm$timeMode];   
      cat1(opts,paste(time.txt[tdm$timeMode], "time for",tdm$unbiasedFunc,":",time.TST,"sec\n"));   
      envT$finals <- cbind(envT$finals,Time.TST=time.TST);    
      envT$theFinals <- envT$finals;
      row.names(envT$theFinals) <- (1:nrow(envT$theFinals));   # get rid of "user.self"
      flush.console();  
  } # if(tdm$nrun>0)
   
  envT;
} # end of function tdmEnvTReport

######################################################################################
# --- DEPRECATED ---
# helper fct for tdmEnvTReport:
#     correct the path in directories of envT$sCList
# correctEnvTDir <- function(envT,tdm) {
#     if (!is.null(tdm$path)) {
#       for (i in 1:length(envT$sCList)) {    
#         # replace the tdm$path-part of opts$dir.* with "./". This workaround necessary when the 
#         # original tuning job was done on another machine, e.g. maanbs03, where the path is 
#         # expanded to /home/wolfgang/fiwa/... and we run this function on a Windows system
#         envT$sCList[[i]]$opts$dir.data = sub(tdm$path,"./",envT$sCList[[i]]$opts$dir.data);
#         envT$sCList[[i]]$opts$dir.txt = sub(tdm$path,"./",envT$sCList[[i]]$opts$dir.txt);
#         envT$sCList[[i]]$opts$dir.Rdata = sub(tdm$path,"./",envT$sCList[[i]]$opts$dir.Rdata);
#         envT$sCList[[i]]$opts$dir.output = sub(tdm$path,"./",envT$sCList[[i]]$opts$dir.output);
#       }     
#     }
# }

###################################################################################################
#'  Sensitivity Report Helper Function
#' 
#' Helper function for the sensitivity report
#'
#' @param B    best solution column
#' @param fit  random forest fit
#' @param roi  internal parameter for the initial region of interest, or: \code{spotConfig$alg.roi}
#' @param nsens number of parameters estimated with sensitivity
#' @return Y
#' 
#' @seealso  \code{\link{tdmEnvTReport}}  \code{\link{tdmEnvTReportSens}} 
#' @export
#' @keywords internal
################################################################################################### 
tdmReportSensY <- function(B,fit,roi,nsens) {	
  Y <- NULL
  for (k in 1:length(B)) {
    BV <- B;
    BV[,k] <- seq(roi[k,"lower"],roi[k,"upper"],length.out=nsens)
    Y <- data.frame(cbind(Y,predict(fit,BV)))
    names(Y)[length(Y)] <- names(B)[k]
  }	
  Y
}

###################################################################################################
# tdmEnvTReportSens: 
#' 
#' Function to generate a report with sensitivity plot.
#'
#' The sensitivity curves are based on a metamodel which is a random forest with 100 trees 
#' fitted to the result points from RES-file. The plot contains: 
#'    x-axis:   ROI for each parameter normalized to [-1,1]
#'    y-axis: 
#'
#' @param spotConfig the configuration list of all spot parameters
#' @seealso  \code{\link{tdmEnvTReport}} 
#' @export
################################################################################################### 
tdmEnvTReportSens <- function(spotConfig) {	
  #require(randomForest); # now via direct call 'randomForest::'
  
  opts <- spotConfig$opts;
  cat2(opts,"  Entering tdmEnvTReportSens\n");
  rawData=spotConfig$alg.currentResult;
  # remove the columns which change over replicates:
  rawData=rawData[,setdiff(names(rawData),c("REP","SEED"))];
  print2(opts,summary(rawData));
  
  mergedData <- stats::aggregate(Y~.,data=rawData,FUN=mean);
  C <- data.frame(mergedData[order(mergedData$Y,decreasing=FALSE),,drop=FALSE]);
  best <-  C[1,,drop=FALSE];


  #spotConfig=spotWriteBest(mergedData, spotConfig);
  #C1=spotConfig$alg.currentBest[nrow(spotConfig$alg.currentBest),]       # WK: added ","
  xNames <- row.names(spotConfig$alg.roi);
  yNames <- spotConfig$alg.resultColumn;
  B <- NULL; 
  nsens=20;             # number of points along the normalized ROI range
  for (i in 1:nsens) {
    # replicate best solution (row 1), but cut away 1st column (Y):   
    B <- rbind(B,data.frame(C[1,xNames]));
  }
  #browser()
  names(B) <- xNames; 	
  fit <- randomForest::randomForest(rawData[xNames], rawData[[yNames]], ntree=100)		
  #fit <- rpart(y ~ ., data= rawB)
  rwb <- cbind(spotConfig$alg.roi,t(B[1,]))     # rwb: roi with 'BEST' column
  names(rwb)[length(rwb)] <- "BEST"
  Y <- tdmReportSensY(B,fit,spotConfig$alg.roi,nsens)
  # scale each ROI to the normalized ROI range [-1,+1]:
  X=seq(-1,1,length.out=nsens)	
  # XP is the location of the SPOT best point for each parameter in the normalized ROI range
  XP = (rwb$BEST-rwb$lower)/(rwb$upper-rwb$lower)*2-1
  XP=rbind(XP,XP);   # matpoints needs at least two rows to plot each column in a different color
  YP = min(Y);  YP=rbind(YP,YP)
  #palette("default")
  cat1(opts," \n")
  cat1(opts,"Sensitivity plot for this ROI:\n")
  print1(opts,rwb)
  cat1(opts," \n")
  cat1(opts,paste("Best solution found with ",nrow(rawData)," evaluations:\n",sep=""))
  print1(opts,C[1,])
  cat1(opts," \n")
  cat1(opts,"Standard deviation of best solution: ")
  y<-rawData[rawData$CONFIG==C[1,"CONFIG"],spotConfig$alg.resultColumn]
  cat1(opts,paste(mean(y),"+-",sd(y),"\n"))
  #finally the plot commands, both for screen and pdf file
  # if(spotConfig$report.io.pdf==TRUE){ #if pdf should be created
  #   pdf(spotConfig$io.pdfFileName) #start pdf creation
  #   matplot(X,Y,type="l",lwd=rep(3,ncol(Y)),cex.axis=1.5,cex.lab=1.5,col=1:ncol(Y),xlab="normalized ROI",main=spotConfig$userConfFileName) 
  #   matpoints(XP,YP,pch=rep(21,ncol(Y)),bg=1:ncol(Y),cex=2)	
  #   legend("topleft",legend=names(Y),lwd=rep(2,ncol(Y)),lty=1:ncol(Y),col=1:ncol(Y),text.col=1:ncol(Y))	
  #   dev.off() #close pdf device
  # }
  # if(spotConfig$report.io.screen==TRUE && spotConfig$io.verbosity>0) #if graphic should be on screen
  # {
  #   dev.new()
    graphics::matplot(X,Y,type="l",lwd=rep(3,ncol(Y)),cex.axis=1.5,cex.lab=1.5,col=1:ncol(Y),xlab="normalized ROI",main=spotConfig$userConfFileName) 
    graphics::matpoints(XP,YP,pch=rep(21,ncol(Y)),bg=1:ncol(Y),cex=2)	
    graphics::legend("topleft",legend=names(Y),lwd=rep(2,ncol(Y)),lty=1:ncol(Y),col=1:ncol(Y),text.col=1:ncol(Y))
  #}
  cat2(opts,"\n Leaving tdmEnvTReportSens\n");
  
  spotConfig;    # not really necessary, nothing changed in spotConfig
}





