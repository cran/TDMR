######################################################################################
# tdmCompleteEval:
#
#' Tuning and unbiased evaluation in a big loop.
#'
#' For each \code{.conf} file in \code{runList} call all tuning algorithms (SPOT, CMA-ES or other) specified in \code{tdm$tuneMethod}
#' (via function \code{\link{tdmDispatchTuner}}). For each tuning process perform one or several runs
#' of \code{tdm$unbiasedFunc} (as many as \code{tdm$umode} has elements). Usually, \code{tdm$unbiasedFunc = \link{unbiasedRun}}.
#'
#Details:
#' Tuning is skipped if the \code{.conf} file does not appear in \code{spotList} or if \code{!(spotStep=="auto")}. In this
#' case it is assumed then that the appropriate \code{.bst} and \code{.res} files exist already.
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
#'  @param runList   vector of \code{.conf} filenames
#'  @param spotList  \code{[NULL]} vector of \code{.conf} filenames for which spot tuning is done.
#'                   If \code{NULL}, then \code{spotList=runList}.
#'  @param spotStep  \code{["auto"]} which step of SPOT to execute (either \code{"auto"} or \code{"rep"}). Entries in this vector are
#'                   cyclically recycled if \code{spotStep} is shorter than \code{runList}.
#'  @param tdm      a list from which we need here the elements
#'     \describe{
#'     \item{\code{mainFunc}}{ name of the DM-function to be called for tuning and for unbiased evaluations. 
#'                             See \code{\link{tdmDefaultsFill}} how a default is found, if \code{mainFunc} is missing}
#      \item{\code{mainCommand}}{ -- deprecated, now constructed from mainFunc -- 
#                              [\code{result <- tdm$mainFunc(opts)}] the command to be called for tuning and unbiased evaluations}
#'     \item{\code{mainFile}}{ if not NULL, source this file and change to the directory of mainFile before starting mainFunc}
#'     \item{\code{tuneMethod}}{vector of tuning method(s) \code{[ "spot", "cmaes", "lhd", "bfgs", "powell" ]} , see \code{\link{spotTuner}},
#'          \code{\link{cmaesTuner}}, \code{\link{lhdTuner}}, \code{\link{bfgsTuner}}, \code{\link{powellTuner}}   }
#'     \item{\code{unbiasedFunc}}{name of the function for unbiased evaluations to call}
#'     \item{\code{umode}}{a vector of strings containing the unbiased resampling strategies
#'          to execute \code{"RSUB", "TST", "CV", "SP_T"}, see \code{\link{unbiasedRun}} }
#'     \item{\code{finalFile}}{filename where to save \code{envT$theFinals}}
#'     \item{\code{withParams}}{[length(runList)==1] Boolean, if \code{=TRUE}: include best parameters as columns in output \code{envT$theFinals}.  
#'			    If \code{=FALSE}: don't (this is appropriate if \code{runList} combines several .conf files which differ in their parameters)}
#'     \item{\code{timeMode}}{ [1] see \code{\link{tdmDefaultsFill}}   }
#'     \item{\code{SPLIT.SEED}}{[NULL] see \code{\link{tdmSplitTestData}}   }
#'     \item{\code{TST.trnFrac}}{[NULL] if not NULL, copy it to opts$TST.trnFrac  }
#'     \item{\code{filenameEnvT}}{[\code{<runList[1]>.RData}] the \code{.RData} file where environment \code{envT} is saved.  }
#'     }
#'
#'   @return environment \code{envT}, containing  the results
#'      \item{res}{ data frame with results from last tuning (one line for each call of \code{tdmStart*})} 
#'      \item{bst}{ data frame with the best-so-far results from last tuning (one line collected after each (SPO) step)}
#'      \item{resGrid}{  list with data frames \code{res} from all tuning runs. Use \cr
#'            \code{envT$getRes(confFile,nExp,theTuner)}  \cr
#'        to retrieve a specific \code{res}. }
#'      \item{bstGrid}{  list with data frames \code{bst} from all tuning runs. Use \cr
#'            \code{envT$getBst(confFile,nExp,theTuner)}  \cr
#'        to retrieve a specific \code{bst}. }
#'      \item{theFinals}{ data frame with one line for each triple \code{confFile,nExp,tuner}, each line contains summary
#'        information about the tuning run in the form: \cr
#'            \code{confFile tuner nExp [params] NRUN NEVAL RGain.bst RGain.* sdR.*} \cr
#'        \code{[params]} is written depending on \code{tdm$withParams}. \cr
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
#'   \code{tdmCompleteEval} and its subfunctions.
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
#'         \item \code{envT$res } is written to a \code{.res} file in directory \code{<tuneMethod>}
#'         \item \code{envT$bst } is written to a \code{.bst} file in directory \code{<tuneMethod>}
#'      }
#'   More precisely: If \code{tdm$fileMode==TRUE} and we tune with \code{tuneMethod="lhd"} while performing  the 3rd experiment 
#'   for \code{.conf} file \code{cpu_01.conf}, then the \code{.res} file \code{lhd/cpu_01_lhd_03.res} is written relative to the directory 
#'   of \code{.conf} file. Analoguously for \code{.bst} file.  \cr
#'
#' Example usages of function tdmCompleteEval are shown in demo/demo03sonar.r, demo/demo03sonar_B.r and demo/demo04cpu.r, accessible via \cr
#' \code{   demo(demo03sonar)},\cr 
#' \code{   demo(demo03sonar_B)} and \cr
#' \code{   demo(demo04cpu)} 
#'
#' @example      demo/demo03sonar.r
#'
#' @seealso   \code{\link{tdmDispatchTuner}}, \code{\link{unbiasedRun}}
#' @author Wolfgang Konen (\email{wolfgang.konen@@fh-koeln.de}), Patrick Koch
#' @export
######################################################################################
tdmCompleteEval <- function(runList,spotList=NULL,spotStep="auto",tdm) {
  tdm <- tdmDefaultsFill(tdm);
  if (is.null(tdm$finalFile)) tdm$finalFile <- sub(".conf",".fin",runList[1],fixed=TRUE);
  if (is.null(spotList)) spotList <- runList;
  tdm$wP <- ifelse(is.null(tdm$withParams), length(runList)==1, tdm$withParams)

  envT <- new.env();      # The environment envT is passed by reference into the inner functions
                          # which means that it can be used a) to transport information back from
                          # those inner functions and b) to transport information to and back even 
                          # for functions like tdmStartOther which are not allowed to have envT in 
                          # their argument list
                          
  envT$bstGrid <- list();
  envT$resGrid <- list();
  envT$sCList <- list();
  envT$theFinals <- NULL;
  envT$runList <- runList;
  envT$tdm <- tdm;
  # envT$getInd: private helper fct for envT$getBst and envT$getRes
  envT$getInd <- function(confFile,nExp,theTuner) {
    indTuner = which(envT$tdm$tuneMethod==theTuner);
    if (length(indTuner)==0) stop(paste("Could not find tuner ",theTuner,"in tdm$tuneMethod"));
    nConf = which(envT$runList==confFile);
    if (length(nConf)==0) stop(paste("Could not find conf file ",confFile,"in envT$runList"));
    if (nExp<1 | nExp>tdm$nExperim) stop(paste("nExp is not in range {1,...,",tdm$nExperim,"}",sep=""));
    ind = indTuner + length(tdm$tuneMethod)*((nExp-1) + tdm$nExperim*(nConf-1));
  }
  # envT$getBst: return from the linear list envT$bstGrid the data frame bst for the triple {confFile,nExp,theTuner}
  envT$getBst <- function(confFile,nExp,theTuner) {
    ind = envT$getInd(confFile,nExp,theTuner);
    envT$bstGrid[[ind]];
  }
  # envT$getRes: return from the linear list envT$resGrid the data frame res for the triple {confFile,nExp,theTuner}
  envT$getRes <- function(confFile,nExp,theTuner) {
    ind = envT$getInd(confFile,nExp,theTuner);
    envT$resGrid[[ind]];
  }
  nTuner <- length(tdm$tuneMethod);
  nRunList <- length(runList);

  #
  # do all necessary file reading **before**  branching into completeEvalConfNexpTuner
  # (completeEvalConfNexpTuner can be in parallel execution branch, where file access might be not possible)
  #
  k=0;
  for (confFile in runList) {
    k=k+1;
    pathConfFile = paste(tdm$path,confFile,sep="");
    if (!file.exists(pathConfFile)) stop(sprintf("Could not find confFile=%s (current dir=%s)",pathConfFile,getwd()));
    envT$sCList[[k]] <- spotGetOptions(srcPath=tdm$theSpotPath,pathConfFile);
    sC <- envT$sCList[[k]]; print(sC$spot.seed); 
    tdm$wP <- checkRoiParams(tdm$wP,confFile,sC,envT);
    pdFile = sC$io.apdFileName;
  	print(pdFile);	
    pdFile = paste(tdm$path,pdFile,sep="");
    if (!file.exists(pdFile)) stop(sprintf("Could not find pdFile=%s (current dir=%s)",pdFile,getwd()));
    opts <- NULL;     # just to make 'R CMD check' happy  (and in case that after sourcing pdFile 'opts' is not there as expected)
  	source(pdFile,local=TRUE);        # read problem design  (here: all elements of list opts)   
		if (is.null(opts))
		  warning(sprintf("%s does not define the required object opts.",pdFile));  	
#    if (tdm$parallelCPUs>1 & sC$spot.fileMode) {
#      warning(sprintf("%s: Should not have spot.fileMode==TRUE in parallel execution. spot.fileMode is set to FALSE.",confFile));
#      envT$sCList[[k]]$spot.fileMode=FALSE;
#    }
    if (!is.null(tdm$TST.trnFrac)) opts$TST.trnFrac=tdm$TST.trnFrac;

    opts=tdmOptsDefaultsSet(opts,path=tdm$path);
 	  envT$sCList[[k]]$opts=opts;
  }
  rm("roiNames1",envir=envT);    # delete this helper var (was needed only temporarily by checkRoiParams above)

  tdmMapDesLoad(envT,tdm); 
  tdmMapDesSpot$load(tdm); 
  #envT$wP <- wP;
  if (!is.null(tdm$mainFile)) source(tdm$mainFile);           # deprecated (sourcing main_TASK should be done in caller of tdmCompleteEval)

  if (tdm$parallelCPUs>1) {
    if (!exists(".Random.seed")) set.seed(42);
    sfExport(list=c(".Random.seed","tdmMapDesSpot",tdm$mainFunc));
  }
  
	#
	# depending on tdm$parallelCPUs, the following lines execute the code either in parallel 
	# or sequential: For each value of indVec the function completeEvalConfNexpTuner is 
	# called. The vectors tuneVec, expeVec, confVec contain for each element of indVec the
	# corresponding value of tuner, experiment number and .conf file, resp.
	# In case of parallel execution, sfSapply will only return after the last parallel job has finished.
	#
	indVec <- 1:(tdm$nExperim*nTuner*nRunList);
	tuneVec <- rep(tdm$tuneMethod,tdm$nExperim*nRunList);
	expeVec <- rep(sort(rep(1:tdm$nExperim,nTuner)),nRunList);
	confVec <- sort(rep(runList,tdm$nExperim*nTuner));
	if (length(indVec)==1 & tdm$parallelCPUs>1) {
	  warning("There is only one job (length(indVec)==1) --> snowfall would not run, so we set tdm$ParallelCPUs=1");
	  tdm$parallelCPUs=1;
  }
  if (tdm$parallelCPUs>1) {
    sappResult <- sfSapply(indVec, fun=completeEvalConfNexpTuner, tuneVec,expeVec,confVec,spotList,spotStep,envT,tdm);
  } else {  		
		sappResult <- sapply(indVec, completeEvalConfNexpTuner, tuneVec,expeVec,confVec,spotList,spotStep,envT,tdm);
  }
  # populate envT with the results returned in matrix sappResult:
  populateEnvT(sappResult,envT,tdm);
  
  ### The older version had a triple for-loop:
  #   for (confFile in runList) {
  #    for (nExp in 1:tdm$nExperim) {    
  #     for (theTuner in tdm$tuneMethod) { 
  #
  #    } # for (theTuner)
  #   } # for (nExp)
  #  } # for (confFile)

  saveEnvT(envT,runList,tdm$filenameEnvT);
  
  print(envT$theFinals);

  envT;
} # function tdmCompleteEval

######################################################################################
  		#------------------------------------------------------------------------------------------------------
      #  Helper function for tdmCompleteEval, called via sapply or sfSapply:
      #  (ind is an index where confFile varies slowest, nExp varies 2nd-slowest and theTuner varies fastest)
      completeEvalConfNexpTuner <- function(ind,tuneVec,expeVec,confVec,spotList,spotStep,envT,tdm) {
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


        if (tdm$fileMode) {  
          tFinalFile <- ifelse(tdm$nExperim>1, sprintf("%s-e%02d%s",sub(".fin","",tdm$finalFile,fixed=TRUE),nExp,".fin"), tdm$finalFile);
          # i.e. if tdm$finalFile="cpu.fin", then tFinalFile="cpu-e02.fin" for nExp=2
          if (file.exists(tFinalFile) & theTuner==tuneVec[1] & confFile==envT$runList[1]) file.remove(tFinalFile);
        } 
       
        if (tdm$fileMode) { 
          # make temporary files so that parallel runs will not interfere: 
          appendix <- sprintf("%s_%02d",theTuner,nExp);
          tConfFile <- copyToTmpfile(confFile,confFile,".conf",appendix);
          tRoiFile <- copyToTmpfile(confFile,sC$io.roiFileName,".roi",appendix);                                        
          #tApdFile <- copyToTmpfile(confFile,sC$io.apdFileName,".apd",appendix);                                        
        } else {
          tConfFile <- confFile;
          tRoiFile <- sC$io.roiFileName;                                        
        }
        sC <- spotGetOptions(srcPath=tdm$theSpotPath,tConfFile);
              # this configures sC$io.bstFileName and sC$io.resFileName with the right endings 
              # (matching to tConfFile); therefore confFile itself should NOT define these filenames (!)
        sC$io.roiFileName <- tRoiFile;
        sC$opts <- envT$sCList[[nConf]]$opts;
        envT$spotConfig <- sC;

        dataObj <- tdmSplitTestData(sC$opts,tdm,nExp);
        if (!is.null(dataObj)) envT$spotConfig$opts$TST.COL = dataObj$TST.COL;    # this column has to be subtracted in main_* from the input variables

        ptm <- proc.time();
        if (tdm$fileMode) {  
          # If .bst and .res files in tuner subdirs exist: copy them into current dir 
          # (they are needed in case spotStep="rep" in the current dir). 
          # If those subdir files don't exist, we assume that .bst and .res file found in 
          # the current dir are the right ones.
          copyFromTunedir(theTuner,sC$io.resFileName);
          copyFromTunedir(theTuner,sC$io.bstFileName);
     
          if (confFile %in% spotList) {
              
              cat(sprintf("*** Starting TUNER %s, spotStep=%s, on task %s ***\n",theTuner,spotStep[i],tConfFile));
              tdmDispatchTuner(theTuner,tConfFile,spotStep[i],tdm,envT,dataObj);
              # tdmDispatchTuner puts its results in envT$bst, envT$res and returns the (extended) spotConfig in envT$tunerVal
              #
              # (If a user-def'd tdmStart*-function does NOT write on envT$bst and envT$res, 
              # but instead writes the corresponding .bst and .res file, then as a 2nd option
              # these results are fetched below with the tdmGetObj-calls and put into envT. 
              # This 2nd option can of course only work if tdm$fileMode==TRUE)
              
              if (spotStep=="auto") {
                # copy .bst and .res files (they contain new results) into the right subdir:
                copyToTunedir(theTuner,sC$io.resFileName);
                copyToTunedir(theTuner,sC$io.bstFileName);
              } else {
                envT$bst <- tdmGetObj(envT$bst,sC$io.bstFileName,theTuner,tdm);
              }
          } 
          else {    # i.e. !(confFile %in% spotList)
              envT$bst <- tdmGetObj(envT$bst,sC$io.bstFileName,theTuner,tdm);
          }    
          envT$res <- tdmGetObj(envT$res,sC$io.resFileName,theTuner,tdm);
        } 
        else {    # i.e. tdm$fileMode==FALSE
              
              cat(sprintf("*** Starting TUNER %s, spotStep=%s, on task %s ***\n",theTuner,spotStep[i],tConfFile));
              tdmDispatchTuner(theTuner,tConfFile,spotStep[i],tdm,envT,dataObj);
              # tdmDispatchTuner puts its results in envT$bst, envT$res and returns the (extended) spotConfig in envT$tunerVal
             
        } # if(tdm$fileMode)        
        #
        # now we have envT$bst and envT$res filled in any case (either from tdmDispatchTuner or from file)
        #
        
        time.txt = c("Proc", "", "Elapsed");
        time.TRN=(proc.time()-ptm)[tdm$timeMode]; opts=list(); opts$VERBOSE=1;  
        cat1(opts,paste(time.txt[tdm$timeMode], "time for tuning with tdmDispatchTuner:",time.TRN,"sec\n"));     
        
        ptm <- proc.time();
        finals <- NULL;
        for (umode in tdm$umode) {
          cat("*** starting",tdm$unbiasedFunc,"for",tConfFile,"with umode=",umode,"***\n");
          cmd=paste("finals <-",tdm$unbiasedFunc,"(tConfFile,envT,dataObj,umode=umode,finals=finals,withParams=tdm$wP,tdm=tdm)",sep="");
          eval(parse(text=cmd));
        }
        time.TST=(proc.time()-ptm)[tdm$timeMode];   
        cat1(opts,paste(time.txt[tdm$timeMode], "time for",tdm$unbiasedFunc,":",time.TST,"sec\n"));   

        finals <- cbind(finals,Time.TST=time.TST,Time.TRN=time.TRN);  
        
        if (tdm$fileMode) {  
          removeTmpfiles(tConfFile);
          if (!file.exists(dirname(tFinalFile))) dir.create(tFinalFile);
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
            if (!file.exists(dirname(tdm$experFile))) dir.create(tdm$experFile);
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
        
        # we return a *list* with the most important elements from environment envT and not envT itself, because 
        # sfSapply (parallel execution) can not deal with environments as return values.
        list( tunerVal=envT$tunerVal    # last tuning result: spotConfig with extensions       
             ,bst=envT$bst              # last tuning result       
             ,res=envT$res              # last tuning result        
             ,result=envT$result        # last tuning result
             ,theFinals=finals   ###tail(envT$theFinals,1)
             );      
  		} # end of function completeEvalConfNexpTuner
  		#------------------------------------------------------------------------------------------------------

######################################################################################
# helper fct for tdmCompleteEval: 
#     Populate the global envT after parallel execution  with snowfall (sfSapply with results in sappResult).
#     For simplicity we use it also after sequential execution (sapply with results in sappResult). 
populateEnvT <- function(sappResult,envT,tdm) {
      nGrid = length(envT$bstGrid);
      for (ind in 1:(tdm$nExperim*length(tdm$tuneMethod)*length(envT$runList))) {
          nGrid = nGrid+1;
          envT$bstGrid[[nGrid]] <- as.data.frame(sappResult["bst",ind][[1]]);
          envT$resGrid[[nGrid]] <- as.data.frame(sappResult["res",ind][[1]]);   
          envT$theFinals <- rbind(envT$theFinals,as.data.frame(sappResult["theFinals",ind][[1]]));
          # "[[1]]" is necessary to avoid prefix "theFinals." in the header names 
      }
      rownames(envT$theFinals) <- (1:nrow(envT$theFinals));
      envT$bst <- as.data.frame(sappResult["bst",ncol(sappResult)][[1]]);
      envT$res <- as.data.frame(sappResult["res",ncol(sappResult)][[1]]);
      envT$tunerVal <- sappResult["tunerVal",ncol(sappResult)][[1]];    # last tuning result
      envT$result <- sappResult["result",ncol(sappResult)][[1]];        # last tuning result
}

######################################################################################
# helper fct for tdmCompleteEval: 
#     Save a small version of environment envT (passed in as thisEnvT) on filenameEnvT
#     (if NULL, use <runList[1]>.rda)
saveEnvT <- function(thisEnvT,runList,filenameEnvT=NULL) {
      envT = list() # new.env(); #            # when saving thisEnvT, we copy the relevant elements to a *list* envT
                                              # and skip the function elements (getBst,getInd,getRes) in envT, because 
                                              # they would also save *their* environment which tends to make the .rda file rather big
      for (ele in setdiff(ls(thisEnvT),c("getBst","getInd","getRes"))) 
        eval(parse(text=paste("envT$",ele," = thisEnvT$",ele,sep="")));
                                              # for the save on .rda file we delete also some potentially voluminous elements 
      envT$result$lastRes$d_train=NULL;       # from the local envT, we want only res and bst objects + describing vars ...
      envT$result$lastRes$d_test=NULL;
      envT$result$lastRes$d_dis=NULL;
      envT$result$lastRes$lastProbs=NULL;
      envT$result$lastRes$lastModel=NULL;
      envT$tunerVal$seq.modelFit=NULL;        # this can be quite big in the case of spotPredictRandomForest
      envT$result$dset=NULL;
      envT$result$TST=NULL;
      envT$spotConfig=NULL;     # versions of spotConfig are contained in envT$sCList[[i]] and envT$tunerVal
      envT$theTuner=NULL;
      envT$nExp=NULL;           # see envT$tdm$nExperim for number of experiments
      if (is.null(filenameEnvT)) filenameEnvT=sub(".conf",".RData",runList[1],fixed=TRUE);

      save(envT,file=filenameEnvT);              
                                              # ... but we leave thisEnvT (which is envT in tdmCompleteEval) untouched
                                              #     and thus return the full envT environment to the caller of tdmCompleteEval
}

######################################################################################
# helper fct for tdmCompleteEval:
#     Check whether each .roi file in envT$runList contains the same design parameters, otherwise set wP=FALSE and issue a warning 
checkRoiParams <- function(wP,confFile,sC,envT) {
      if (wP) {
        if (confFile==envT$runList[1]) {
          envT$roiNames1 = rownames(sC$alg.roi);
        } else {
          roiNames = rownames(sC$alg.roi);
          if (length(roiNames)!=length(envT$roiNames1)) {
            wP=FALSE;
          } else {
            if (any(roiNames!=envT$roiNames1)) wP=FALSE;
          }
        }
        if (wP==FALSE) warning("The design parameters in differents ROI files are different --> TDMR sets tdm$withParams to FALSE (no param columns in envT$theFinals)");      
      }
      wP;
}
  
######################################################################################
# some helper fcts for tdmCompleteEval needed only in case tdm$fileMode==TRUE
copyFromTunedir <- function(theTuner,File) {          
      tfile <- paste(theTuner,File,sep="/");
      if (file.exists(tfile)) {
        cat(sprintf("Copying %s to ./%s\n",tfile,File));
        file.copy(tfile,".",overwrite=TRUE);
      }
}
copyToTunedir <- function(theTuner,File) {          
      if (!file.exists(theTuner)) dir.create(theTuner)
      tt=file.copy(File,theTuner,overwrite=TRUE)
      if (tt) cat(sprintf("Copying %s to %s/%s\n",File,theTuner,File)) else
        warning(sprintf("Failure when copying %s to %s/%s\n",File,theTuner,File)); 
}
copyToTmpfile <- function(confFile,File,Suffix,Appendix) {
      tFile <- paste(sub(".conf","",confFile,fixed=TRUE),"_",Appendix,Suffix,sep="")
      file.copy(File,tFile,overwrite=TRUE);
      tFile;
}
removeTmpfiles <- function(tConfFile) {
      for (suf in c("apd","aroi","bst","conf","des","res","roi"))  {
        tFile <- paste(strsplit(tConfFile,"conf"),suf,sep="");
        if (file.exists(tFile)) file.remove(tFile);
      }
}
   

