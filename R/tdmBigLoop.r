######################################################################################
# tdmBigLoop:
#
#' Tuning and unbiased evaluation in a big loop.
#'
#' For each \code{.conf} file in \code{tdm$runList} call all tuning algorithms (SPOT, CMA-ES or other) specified in \code{tdm$tuneMethod}
#' (via function \code{\link{tdmDispatchTuner}}). After each tuning process perform one or several runs
#' of \code{tdm$unbiasedFunc} (as many as \code{tdm$umode} has elements). Usually, \code{tdm$unbiasedFunc = \link{unbiasedRun}}. \cr
#' Each of these experiments is repeated tdm$nEperim times. Thus we have for each tripel \cr
#'        \code{(confFile,nExp,theTuner)} \cr
#' a tuning result with \cr
#'        \code{confFile in tdm$runList} \cr
#'        \code{nExp in 1,...,tdm$nExperim} \cr
#'        \code{theTuner in tdm$tuneMethod} \cr
#' 
#Details:
#' \code{tdm} refers to \code{envT$tdm}.
#' \cr\cr
#' Tuning is skipped if the \code{.conf} file does not appear in \code{tdm$spotList} or if \code{spotStep=="rep"}. In this
#' case it is assumed then that \code{envT$bstGrid} and \code{envT$resGrid} contain the appropriate data frames already.
#' See \code{\link{tdmEnvTAddBstRes}} on how to fill \code{envT$bstGrid} and \code{envT$resGrid}  from an \code{.RData} file.
#'
#' The available tuning algorithms (tuners) are 
#'      \itemize{
#'      \item{\code{\link{spotTuner}}:  Call \code{\link{spot}}.   }
#'      \item{\code{\link{lhdTuner}}:  Perform a parameter tuning using a Latin hypercube design (LHD) 
#'            for obtaining best design points. LHD is performed by configuring SPOT 
#'            in such a way that all the budget is used for the initial design (usually LHD). }
#'      \item{\code{\link{cmaesTuner}}:  Perform a parameter tuning by CMA-ES, using the *R*-implementation 
#'            (package \code{cmaes} by Olaf Mersmann).  }
#'      \item{\code{\link{cma_jTuner}}:  Perform a parameter tuning by CMA-ES, using the *Java* 
#'            implementation by Niko Hansen.    }
#'      \item{\code{\link{bfgsTuner}}:   Perform a parameter tuning by Broyden, Fletcher, Goldfarb and Shanno (BFGS) method.
#'            The L-BFGS-B version allowing box constraints is used.  }
#'      \item{\code{\link{powellTuner}}:  Perform a parameter tuning by Powell's UObyQA algorithm 
#'            (unconstrained optimization by quadratic approximation), see R-package \code{powell}.   } 
#'      }
#'
#'  @param envT      an environment containing on input at least the element \code{tdm} (a list with general settings for TDMR, 
#'                   see \code{\link{tdmDefaultsFill}}), which has as extra elements  
#'     \describe{
#'     \item{\code{tdm$runList}}{ vector of \code{.conf} filenames }
#'     \item{\code{tdm$spotList}}{ \code{[NULL]} vector of \code{.conf} filenames for which spot tuning is done. 
#'                             If \code{NULL}, then \code{spotList=runList} }
#'     }
#'  @param spotStep  \code{["auto"]} which step of SPOT to execute (either \code{"auto"} or \code{"rep"}). Entries in this vector are
#'                   cyclically recycled if \code{spotStep} is shorter than \code{runList}.
#'
#'  @return environment \code{envT}, containing  the results
#'      \item{res}{ data frame with results from last tuning (one line for each call of \code{tdmStart*})} 
#'      \item{bst}{ data frame with the best-so-far results from last tuning (one line collected after each (SPO) step)}
#'      \item{resGrid}{  list with data frames \code{res} from all tuning runs. Use \cr
#'            \code{envT$getRes(confFile,nExp,theTuner)}  \cr
#'        to retrieve a specific \code{res}. }
#'      \item{bstGrid}{  list with data frames \code{bst} from all tuning runs. Use \cr
#'            \code{envT$getBst(confFile,nExp,theTuner)}  \cr
#'        to retrieve a specific \code{bst}. }
#'      \item{theFinals}{ data frame with one line for each triple \code{(confFile,nExp,tuner)}, each line contains summary
#'        information about the tuning run in the form: \cr
#'            \code{confFile tuner nExp [params] NRUN NEVAL RGain.bst RGain.* sdR.*} \cr
#'        where \code{[params]} is written depending on \code{tdm$withParams}. \cr
#'        \code{NRUN} is the number of unbiased evaluation runs. \cr
#'        \code{NEVAL} is the number of function evaluations (model builds) during tuning. \cr
#'        \code{RGain} denotes the relative gain on a certain data set: the actual gain achieved with the model 
#'        divided by the maximum gain possible for the current cost matrix and the current data set. This is for classification
#'        tasks, in the case of regression each \code{RGain.*} is replaced by \code{RMAE.*}, the relative mean absolute error. \cr
#'        Each 'sdR.' denotes the standard deviation of the preceeding RGain or RMAE. \cr
#'        RGain.bst is the best result during tuning obtained on the training-validation data. RGain.avg is the average result 
#'        during tuning. The following pairs {RGain.* sdR.*} are the results of one or several unbiased evaluations on the test data
#'        where '*' takes as many values as there are elements in \code{tdm$umode} (the possible values are explained in 
#'        \code{\link{unbiasedRun}}).  
#'        }
#'      \item{result}{ object of class TDMclassifier or TDMregressor. This is a list with results from \code{tdm$mainFunc} 
#'          as called in the last unbiased evaluation using the best parameters found during tuning. 
#'          Use \code{\link[=print.TDMclassifier]{print}(envT$result)} to get more info on such an object of class TDMclassifier. \cr
#'          See \code{\link{tdmClassifyLoop}} or \code{\link{tdmRegressLoop}} for further info on TDMclassifier or TDMregressor, resp. }
#'      \item{tunerVal}{ an object with the return value from the last tuning process. For every tuner, this is the list 
#'          \code{spotConfig}, containing the SPOT settings plus the TDMR settings in elements \code{opts} and \code{tdm}. Every tuner 
#'          estends this list by \code{tunerVal$alg.currentResult} and \code{tunerVal$alg.currentBest}, see \code{\link{tdmDispatchTuner}}.
#'          In addition, each tuning method might add specific elements to the list, see the description of each tuner. }
#'   Environment \code{envT} contains further elements, but they are only relevant for the internal operation of 
#'   \code{tdmBigLoop} and its subfunctions.
#'
#' @note Side effects:
#'   Irrespective of the value of \code{tdm$fileMode}, 
#'     \itemize{
#'         \item a compressed version of \code{envT } is saved to file \code{tdm$filenameEnvT} (default: \code{<runList[1]>.RData}), 
#'               relative to  the directory of the \code{.conf} file. 
#'     }
#' 
#'   If \code{tdm$fileMode==TRUE}, more files are written relative to  the directory of the \code{.conf} file:
#'     \itemize{
#'         \item \code{envT$theFinals } is written to file \code{tdm$finalFile} and appended to \code{tdm$experFile}
#'      }
#'   If \code{tdm$finalFile==NULL}, then it is set to \code{sub(".conf",".fin",runList[1]}.  \cr
#'   If \code{tdm$experFile==NULL}, then nothing is appended to any experiment file.
#'
#' Example usages of function \code{tdmBigLoop} are shown with \cr
#' \code{   demo(demo03sonar)},\cr 
#' \code{   demo(demo03sonar_B)} and \cr
#' \code{   demo(demo04cpu)} \cr
#' where the corresponding R-sources are in directory \code{demo}.
#'
#' @example      demo/demo03sonar.r
#'
#' @seealso   \code{\link{tdmCompleteEval}} (older, more complicated version), \code{\link{tdmDispatchTuner}}, \code{\link{unbiasedRun}}
#' @author Wolfgang Konen (\email{wolfgang.konen@@fh-koeln.de}), Patrick Koch
#' @export
######################################################################################
tdmBigLoop <- function(envT,spotStep="auto") {
                            # The environment envT is passed by reference into the inner functions
                            # which means that it can be used a) to transport information back from
                            # those inner functions and b) to transport information to and back even 
                            # for functions like tdmStartOther which are not allowed to have envT in 
                            # their argument list
  tdm <- envT$tdm;
  tdm <- tdmMapDesLoad(tdm);

  if (!is.null(tdm$mainFile)) source(tdm$mainFile);           # deprecated (sourcing main_TASK should be done in caller of tdmBigLoop)
  nTuner <- length(tdm$tuneMethod);
  nRunList <- length(tdm$runList);

  # 
  # if tdm$parallelCPUs>1, set up everything for parallel execution
  #
  if (tdm$parallelCPUs>1) prepareParallelExec(tdm);

	# depending on tdm$parallelCPUs, the following lines execute the code either in parallel 
	# or sequential: For each value of indVec the function bigLoopStep is 
	# called. The vectors tuneVec, expeVec, confVec contain for each element of indVec the
	# corresponding value of tuner, experiment number and .conf file, resp.
	# In case of parallel execution, sfSapply will only return after the last parallel job has finished.
	#
	indVec <- 1:(tdm$nExperim*nTuner*nRunList);
	tuneVec <- rep(tdm$tuneMethod,tdm$nExperim*nRunList);
	expeVec <- rep(sort(rep(1:tdm$nExperim,nTuner)),nRunList);
	confVec <- sort(rep(tdm$runList,tdm$nExperim*nTuner));
	if (length(indVec)==1 & tdm$parallelCPUs>1) {
	  warning("There is only one job (length(indVec)==1) --> snowfall would not run, so we set tdm$ParallelCPUs=1");
	  tdm$parallelCPUs=1;
  }

  if (tdm$parallelCPUs>1) {
    sappResult <- sfSapply(indVec, fun=bigLoopStep, tuneVec,expeVec,confVec,tdm$spotList,spotStep,envT,tdm);
  } else {  		
		sappResult <- sapply(indVec, bigLoopStep, tuneVec,expeVec,confVec,tdm$spotList,spotStep,envT,tdm);
  }
  # populate envT with the results returned in matrix sappResult:
  populateEnvT(sappResult,envT,tdm);
  
  saveEnvT(envT,tdm$runList,tdm$filenameEnvT);
  
  if (nrow(envT$theFinals)>0) {
    print(envT$theFinals);
  } else {
    cat("Note: No rows in data frame envT$theFinals\n");
  }

  envT;
} # function tdmBigLoop

      ######################################################################################
  		#------------------------------------------------------------------------------------------------------
      #  bigLoopStep: helper function for tdmBigLoop, called via sapply or sfSapply:
      #  (ind is an index where confFile varies slowest, nExp varies 2nd-slowest and theTuner varies fastest)
      bigLoopStep <- function(ind,tuneVec,expeVec,confVec,spotList,spotStep,envT,tdm) {
        theTuner = tuneVec[ind];
        nExp = expeVec[ind];
        confFile = confVec[ind];
        nConf = which(confFile==envT$runList);
        i <- (nConf-1) %% length(spotStep) + 1; # i is an index which cyclically re-uses entries      
                                                # from vector spotStep, if it is shorter than runList
        print(c(ind,confFile,nExp,theTuner));
        envT$spotConfig <- sC <- envT$sCList[[nConf]]; # spotGetOptions(srcPath=tdm$theSpotPath,confFile);
        envT$theTuner <- theTuner;
        envT$nExp <- nExp;
        envT$bst <- NULL;
        envT$res <- NULL; 
        if (spotStep=="rep" | spotStep=="report" | !(confFile %in% spotList)) {
            envT$bst = envT$getBst(confFile,nExp,theTuner);
            envT$res = envT$getRes(confFile,nExp,theTuner);
        }

        if (tdm$fileMode) {  
          if (is.null(tdm$finalFile)) tdm$finalFile = sub(".conf",".fin",confFile);
          tFinalFile <- ifelse(tdm$nExperim>1, sprintf("%s-e%02d%s",sub(".fin","",tdm$finalFile,fixed=TRUE),nExp,".fin"), tdm$finalFile);
          # i.e. if tdm$finalFile="cpu.fin", then tFinalFile="cpu-e02.fin" for nExp=2
          if (file.exists(tFinalFile) & theTuner==tuneVec[1] & confFile==envT$runList[1]) file.remove(tFinalFile);
        } 
        # NEW 09/2012: always operate SPOT with spot.fileMode=FALSE (take everything from  envT$spotConfig)
        envT$spotConfig$spot.fileMode=FALSE;
       
        #
        # this is the preferred place to read the data and split them into test data and train/vali data
        #
        dataObj <- tdmSplitTestData(sC$opts,tdm,nExp);
        if (!is.null(dataObj)) envT$spotConfig$opts$TST.COL = dataObj$TST.COL;    # this column has to be subtracted in main_* from the input variables

        ptm <- proc.time();
        if (confFile %in% spotList) {
            # this is for the case spotStep=="rep":
            envT$spotConfig$alg.currentResult <- envT$res;
            envT$spotConfig$alg.currentBest <- envT$bst;	
            
            cat(sprintf("*** Starting TUNER %s, spotStep=%s, on task %s ***\n",theTuner,spotStep[i],confFile));
            tdmDispatchTuner(theTuner,confFile,spotStep[i],tdm,envT,dataObj);
            # If spotStep[i]=="auto" then tdmDispatchTuner runs the tuning process, puts its results in envT$bst, envT$res 
            # and returns the (extended) spotConfig in envT$tunerVal.
            # If spotStep[i]=="rep" then tdmDispatchTuner expects that envT$bst, envT$res contain already the right data frames 
            # and it runs just the reporting step of SPOT.
        } 
        else {    # i.e. !(confFile %in% spotList)
            # we assume that envT$bst and envT$res contain already the right data frames (filled in from a previous setting)
        }    
        
        #
        # now we have envT$bst and envT$res filled in any case (either from tdmDispatchTuner or from a previous setting)
        #
        if (is.null(envT$bst)) stop("No contents for data frame envT$bst");
        if (is.null(envT$res)) stop("No contents for data frame envT$res");
        
        if (is.null(tdm$timeMode)) stop("tdm$timeMode is not set (NULL). Consider 'tdm <- tdmDefaultsFill(tdm)' to set all defaults");
        time.txt = c("Proc", "System", "Elapsed");
        time.TRN=(proc.time()-ptm)[tdm$timeMode]; opts=list(); opts$VERBOSE=1;  
        cat1(opts,paste(time.txt[tdm$timeMode], "time for tuning with tdmDispatchTuner:",time.TRN,"sec\n"));     
     
        ptm <- proc.time();
        finals <- NULL;
        if (tdm$nrun>0) {
          for (umode in tdm$umode) {
            cat("*** starting",tdm$unbiasedFunc,"for",confFile,"with umode=",umode,"***\n");
            cmd=paste("finals <-",tdm$unbiasedFunc,"(confFile,envT,dataObj,umode=umode,finals=finals,withParams=envT$wP,tdm=tdm)",sep="");
            eval(parse(text=cmd));
          }
          time.TST=(proc.time()-ptm)[tdm$timeMode];   
          cat1(opts,paste(time.txt[tdm$timeMode], "time for",tdm$unbiasedFunc,":",time.TST,"sec\n"));   
          finals <- cbind(finals,Time.TST=time.TST,Time.TRN=time.TRN);  
          
          if (tdm$fileMode) {  
            #removeTmpfiles2(confFile);
            if (!file.exists(dirname(tFinalFile))) {
              success = dir.create(dirname(tFinalFile));     
              if (!success) stop(sprintf("Could not create dirname(tFinalFile)=%s",dirname(tFinalFile)));
            }
        		colNames = ifelse(file.exists(tFinalFile),FALSE,TRUE);
        		write.table(finals
        				, file = tFinalFile
        				, col.names= colNames
        				, row.names= FALSE
        				, append = !colNames
        				, sep = " ",
        				, quote = FALSE
        				, eol = "\n"
        		);
        		if (!is.null(tdm$experFile)) {
              if (!file.exists(dirname(tdm$experFile))) {
                success = dir.create(dirname(tdm$experFile));     
                if (!success) stop(sprintf("Could not create dirname(tdm$experFile)=%s",dirname(tdm$experFile)));
              }
          		colNames = ifelse(file.exists(tdm$experFile),FALSE,TRUE);
          		write.table(finals             # multiple execution of same experiment (nExperim>1 in script_all.R)
          				, file = tdm$experFile
          				, col.names= colNames
          				, row.names= FALSE
          				, append = !colNames
          				, sep = " ",
          				, quote = FALSE
          				, eol = "\n"
          		);
        		}
          } # if(tdm$fileMode)
          flush.console(); 
        } # if(tdm$nrun>0)
         
        # we return a *list* with the most important elements from environment envT and not envT itself, because 
        # sfSapply (parallel execution) can not deal with environments as return values.
        list( tunerVal=envT$tunerVal    # last tuning result: spotConfig with extensions       
             ,bst=envT$bst              # last tuning result       
             ,res=envT$res              # last tuning result  
             ,result=envT$result        # last tuning result
             ,roi=envT$tunerVal$alg.roi      
             ,theFinals=finals   ###tail(envT$theFinals,1)
             );      
  		} # end of function bigLoopStep
  		#------------------------------------------------------------------------------------------------------

######################################################################################
# helper fct for tdmBigLoop: 
#     Populate the global envT after parallel execution  with snowfall (sfSapply with results in sappResult).
#     For simplicity we use it also after sequential execution (sapply with results in sappResult). 
populateEnvT <- function(sappResult,envT,tdm) {
      nGrid = length(envT$bstGrid);
      for (ind in 1:(tdm$nExperim*length(tdm$tuneMethod)*length(envT$runList))) {
          nGrid = nGrid+1;
          envT$bstGrid[[nGrid]] <- as.data.frame(sappResult["bst",ind][[1]]);
          envT$resGrid[[nGrid]] <- as.data.frame(sappResult["res",ind][[1]]);   
          envT$roiGrid[[nGrid]] <- as.data.frame(sappResult["roi",ind][[1]]);   
          envT$theFinals <- rbind(envT$theFinals,as.data.frame(sappResult["theFinals",ind][[1]]));
          # "[[1]]" is necessary to avoid prefix "theFinals." in the header names 
      }
browser()     
      if (!is.null(envT$theFinals)) if(nrow(envT$theFinals)>0)
          rownames(envT$theFinals) <- (1:nrow(envT$theFinals));
      envT$bst <- as.data.frame(sappResult["bst",ncol(sappResult)][[1]]);
      envT$res <- as.data.frame(sappResult["res",ncol(sappResult)][[1]]);
      envT$tunerVal <- sappResult["tunerVal",ncol(sappResult)][[1]];    # last tuning result
      envT$result <- sappResult["result",ncol(sappResult)][[1]];        # last tuning result
}

######################################################################################
# helper fct for tdmBigLoop: 
#     Save a small version of environment envT (passed in as thisEnvT) on filenameEnvT
#     (if NULL, use <runList[1]>.RData).
#     If savePredictions is true, the elements envT$result$predictions, envT$result$predProbList, envT$result$lastRes$predProb are
#     saved. The default is savePredictions==FALSE.
saveEnvT <- function(thisEnvT,runList,filenameEnvT=NULL,savePredictions=FALSE) {
      envT = list() # new.env(); #            # when saving thisEnvT, we copy the relevant elements to a *list* envT
                                              # and skip the function elements (getBst,getInd,getRes) in envT, because 
                                              # they would also save *their* environment which tends to make the .RData file rather big
      for (ele in setdiff(ls(thisEnvT),c("getBst","getInd","getRes"))) 
        eval(parse(text=paste("envT$",ele," = thisEnvT$",ele,sep="")));
                                              # for the save on .RData file we delete also some potentially voluminous elements 
      envT$result$lastRes$d_train=NULL;       # from the local envT, we want only res and bst objects + describing vars ...
      envT$result$lastRes$d_test=NULL;
      envT$result$lastRes$d_dis=NULL;
      envT$result$lastRes$lastProbs=NULL;
      envT$result$lastRes$lastModel=NULL;
      envT$result$dset=NULL;
      envT$result$TST=NULL;
      if (!savePredictions) {
        envT$result$predictions=NULL;     
        envT$result$predProbList=NULL;  
        envT$result$lastRes$predProb=NULL;   
      }
      envT$tunerVal$seq.modelFit=NULL;        # this can be quite big in the case of spotPredictRandomForest
      envT$spotConfig=NULL;     # versions of spotConfig are contained in envT$sCList[[i]] and envT$tunerVal
      envT$theTuner=NULL;
      envT$nExp=NULL;           # see envT$tdm$nExperim for number of experiments
      if (is.null(filenameEnvT)) filenameEnvT=sub(".conf",".RData",runList[1],fixed=TRUE);

      save(envT,file=filenameEnvT);              
                                              # ... but we leave thisEnvT (which is envT in tdmBigLoop) untouched
                                              #     and thus return the full envT environment to the caller of tdmBigLoop
}

######################################################################################
# helper fct for tdmBigLoop 
removeTmpfiles2 <- function(confFile) {
      #for (suf in c("apd","aroi","bst","conf","des","res","roi"))  {    # old version (removeTmpfiles)
      for (suf in c("aroi","bst","des","res"))  {
        tFile <- paste(strsplit(confFile,"conf"),suf,sep="");
        if (file.exists(tFile)) file.remove(tFile);
      }
}
   
######################################################################################
# helper function for tdmBigLoop and tdmCompleteEval, only needed in case tdmParallelCPUs>1 
# 
# set up everything for parallel execution
#
prepareParallelExec <- function(tdm) 
{
    #require(snow)
    require(snowfall);

    createSourcePath <- function(sourceFileName){
      normalizePath(paste(tdm$tdmPath,"R",sourceFileName, sep="/"));
    }
    
    parallelCPUs = tdm$parallelCPUs;
    sfInit(parallel=TRUE, cpus=parallelCPUs, type="SOCK" )
    sfExport(list=c("tdm"))
    
    if (!exists(".Random.seed")) set.seed(42);
    sfExport(list=".Random.seed");
    sfExport(list=c(tdm$mainFunc,tdm$parallelFuncs));
    if (is.null(tdm$tdmPath)) {
        cat("sfLibrary for installed library TDMR \n");
        sfLibrary("TDMR",character.only=TRUE);
    } else {
        cat("Sourcing and sfExport for TDMR from R files in",tdm$tdmPath,"\n");
        source(paste(tdm$tdmPath,"source.tdm.r",sep="/"),local=TRUE);
        if (is.null(tdm$theSpotPath)) tdm$theSpotPath <- NA;
        if (is.null(tdm$theRsfaPath)) tdm$theRsfaPath <- NA;
        sfLibrary("randomForest",character.only=TRUE);
        sfLibrary("e1071",character.only=TRUE);        # svm(), Naive Bayes
        #sfLibrary("matlab",character.only=TRUE);      # repmat() etc., for tdmParaBootstrap.r - now deprecated 12/2011
        if (is.na(tdm$theSpotPath)) {
            sfLibrary("SPOT",character.only=TRUE);     # load SPOT from the installed library (package version)
        } else {
            oldwd=getwd(); setwd(tdm$theSpotPath);
            for (f in dir()) sfSource(f);
            setwd(oldwd);
        }
        if (is.na(tdm$theRsfaPath)) {
            sfLibrary("rSFA",character.only=TRUE);     # load rSFA from the installed library (package version)
        } else {
            oldwd=getwd(); setwd(tdm$theRsfaPath);
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
    
        sfSource(createSourcePath("tdmBigLoop.r"))
        sfSource(createSourcePath("tdmCompleteEval.r"))
        sfSource(createSourcePath("tdmDefaultsFill.r"))
        sfSource(createSourcePath("tdmDispatchTuner.r"))
        sfSource(createSourcePath("tdmEnvTMakeNew.r"))
        sfSource(createSourcePath("tdmGetObj.r"))
        sfSource(createSourcePath("tdmMapDesign.r"))
        sfSource(createSourcePath("tdmPlotResMeta.r"))
        sfSource(createSourcePath("tdmROCR.r"))
        sfSource(createSourcePath("tdmSplitTestData.r"))
        sfSource(createSourcePath("tdmStartSpot.r"))
        sfSource(createSourcePath("unbiasedRun.r"))
        sfSource(createSourcePath("unbiasedBestRun_O.r"))

        sfExport(list=c("tdmRandomSeed"));
    }
} # prepareParallelExec()
  
  

