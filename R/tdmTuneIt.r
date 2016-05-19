######################################################################################
# tdmTuneIt:
#
#' Tuning and unbiased evaluation (single tuning).
#'
#' For the first \code{.conf} file in \code{tdm$runList} call the first tuning algorithm
#' in \code{tdm$tuneMethod} (via function \code{\link{tdmDispatchTuner}}). 
#' After tuning perform with the best parameters a run of \code{tdm$unbiasedFunc}
#' (usually \code{\link{unbiasedRun}}). \cr
#' This experiment is repeated \code{tdm$nExperim} times. 
#' 
#Details:
#' \code{tdmTuneIt} differs from \code{\link{tdmBigLoop}} in that it processes only one \code{.conf} 
#' file and that it has \code{dataObj} as a mandatory calling parameter. This simplifies the data 
#' flow and is thus less error-prone.
#' \cr\cr
#' \code{tdm} refers to \code{envT$tdm}.
#' \cr\cr
#' Tuning is skipped if \code{spotStep=="rep"}. In this case it is assumed then that 
#' \code{envT$bstGrid} and \code{envT$resGrid} contain the appropriate data frames already.
#' See \code{\link{tdmEnvTAddBstRes}} on how to fill \code{envT$bstGrid} and \code{envT$resGrid}  from an \code{.RData} file.
#'
#' See Details in \code{\link{tdmBigLoop}} for the list of avaialble tuners.
#'
#' @param envT      an environment containing on input at least the element \code{tdm} (a list with general settings for TDMR, 
#'                   see \code{\link{tdmDefaultsFill}}), which has at least the elements  
#'     \describe{
#'     \item{\code{tdm$runList}}{ \code{.conf} filename }
#'     \item{\code{tdm$tuneMethod}}{ the tuner }
#'     }
#' @param spotStep  \code{["auto"]} which step of SPOT to execute (either \code{"auto"} or \code{"rep"}).
#' @param dataObj   object of class \code{\link{TDMdata}} (constructed here with the help of 
#'      \code{\link{tdmSplitTestData}}).
#'
#' @return environment \code{envT}, containing  the results
#'      \item{res}{ data frame with results from last tuning (one line for each call of \code{tdmStart*})} 
#'      \item{bst}{ data frame with the best-so-far results from last tuning (one line collected after each (SPO) step)}
#'      \item{resGrid}{  list with data frames \code{res} from all tuning runs. Use \cr
#'            \code{envT$getRes(envT,confFile,nExp,theTuner)}  \cr
#'        to retrieve a specific \code{res}. }
#'      \item{bstGrid}{  list with data frames \code{bst} from all tuning runs. Use \cr
#'            \code{envT$getBst(envT,confFile,nExp,theTuner)}  \cr
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
#'      \item{result}{ object of class \code{\link{TDMclassifier}} or \code{\link{TDMregressor}}. This is a list with results from \code{tdm$mainFunc} 
#'          as called in the last unbiased evaluation using the best parameters found during tuning. 
#'          Use \code{\link[=print.TDMclassifier]{print}(envT$result)} to get more info on such an object of class \code{\link{TDMclassifier}}.  }
#'      \item{tunerVal}{ an object with the return value from the last tuning process. For every tuner, this is the list 
#'          \code{spotConfig}, containing the SPOT settings plus the TDMR settings in elements \code{opts} and \code{tdm}. Every tuner 
#'          extends this list by \code{tunerVal$alg.currentResult} and \code{tunerVal$alg.currentBest}, see \code{\link{tdmDispatchTuner}}.
#'          In addition, each tuning method might add specific elements to the list, see the description of each tuner. }
#'   Environment \code{envT} contains further elements, but they are only relevant for the internal operation of 
#'   \code{tdmBigLoop} and its subfunctions.
#'
#' @note Side effects:
#'   Irrespective of the value of \code{tdm$fileMode}, 
#'     \itemize{
#'         \item a compressed version of \code{envT} is saved to file \code{tdm$filenameEnvT} (default: \code{<runList[1]>.RData}), 
#'               relative to  the directory of the \code{.conf} file. 
#'     }
#'   If \code{tdm$U.saveModel==TRUE}, then \code{envT$result$lastRes$lastModel} (the last trained model) will be saved to \code{tdm$filenameEnvT}. 
#'   The default is \code{tdm$U.saveModel==TRUE} (with \code{tdm$U.saveModel==FALSE} smaller \code{.RData} files).
#' 
#'   If \code{tdm$fileMode==TRUE}, more files are written relative to  the directory of the \code{.conf} file:
#'     \itemize{
#'         \item \code{envT$theFinals } is written to file \code{tdm$finalFile} 
#'         \item \code{envT$theFinals } is appended to \code{tdm$experFile}
#'      }                                                                                                           
#'   If \code{tdm$finalFile==NULL}, then it is set to \code{sub(".conf",".fin",runList[1])}.  \cr
#'   If \code{tdm$experFile==NULL}, then nothing is appended to any experiment file.
#'
#' Example usages of function \code{tdmBigLoop} are shown in \cr
#' \tabular{ll}{
#'    \tab \code{   demo(demo03sonar)} \cr 
#'    \tab \code{   demo(demo03sonar_B)} \cr
#'    \tab \code{   demo(demo04cpu)} \cr
#' }
#' where the corresponding R-sources are in directory \code{demo}.
#'
#' @examples
#' #*# This demo shows a complete tuned data mining process (level 3 of TDMR) where 
#' #*# the data mining task is the classification task SONAR (from UCI repository, 
#' #*# http://archive.ics.uci.edu/ml/datasets/Connectionist+Bench+%28Sonar,+Mines+vs.+Rocks%29).
#' #*# The data mining process is in main_sonar.r, which calls tdmClassifyLoop and tdmClassify
#' #*# with Random Forest as the prediction model. 
#' #*# The three parameter to be tuned are CUTOFF1, CLASSWT2 and XPERC, as specified 
#' #*# in file sonar_04.roi. The tuner used here is LHD.  
#' #*# Tuning runs are rather short, to make the example run quickly. 
#' #*# Do not expect good numeric results. 
#' #*# See demo/demo03sonar_B.r for a somewhat longer tuning run, with two tuners SPOT and LHD.
#' 
#' ## set working directory (dir with .apd, .conf and main_*.r file)
#' path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
#' source(paste(path,"main_sonar.r",sep="/"));    
#' 
#' ## control settings for TDMR
#' tdm <- list( mainFunc="main_sonar"
#'            , runList = c("sonar_04.conf")
#'            , umode="CV"              # { "CV" | "RSUB" | "TST" | "SP_T" }
#'            , tuneMethod = c("lhd")
#'            , filenameEnvT="exBigLoop.RData"   # file to save environment envT (in dir 'path')
#'            , nrun=1, nfold=2         # repeats and CV-folds for the unbiased runs
#'            , nExperim=1
#'            , parallelCPUs=1
#'            , parallelFuncs=c("readCmdSonar")
#'            , optsVerbosity = 0       # the verbosity for the unbiased runs
#'            );
#' ## Each element of tdm$runList has the settings for one tuning process (e.g. 
#' ##    - auto.loop.steps = number of SPOT generations       
#' ##    - auto.loop.evals = budget of model building runs and 
#' ##    - io.roiFileName = "sonar_04.roi"
#' ## ). 
#' 
#' spotStep = "auto";   
#' source(paste(path,"start_tuneIt.r",sep="/"),chdir=TRUE);    # change dir to 'path' while sourcing
#'
#' @seealso   \code{\link{tdmBigLoop}}, \code{\link{tdmDispatchTuner}}, \code{\link{unbiasedRun}}
#' @author Wolfgang Konen (\email{wolfgang.konen@@th-koeln.de})
#' @export
######################################################################################
tdmTuneIt <- function(envT,spotStep="auto",dataObj) {
                            # The environment envT is passed by reference into the inner functions
                            # which means that it can be used a) to transport information back from
                            # those inner functions and b) to transport information to and back even 
                            # for functions like tdmStartOther which are not allowed to have envT in 
                            # their argument list
  
  tdm <- envT$tdm;
  tdm <- tdmMapDesLoad(tdm);

  if (is.null(envT$getBst)) envT <- tdmEnvTAddGetters(envT);
  # just in case the user loaded envT via load("myFile.Rdata") and not tdmEnvTLoad("myFile.Rdata") 
      
	# depending on tdm$parallelCPUs, the following lines execute the code either in parallel 
	# or sequential: For each value of indVec the function bigLoopStep is 
	# called. The vector expeVec contains for each element of indVec the
	# corresponding experiment number.
	# In case of parallel execution, parSapply will only return after the last parallel job has finished.
	#
	indVec <- 1:(tdm$nExperim);
	theTuner <- tdm$tuneMethod[1];
	expeVec <- 1:tdm$nExperim;
	confFile <- tdm$runList[1];
	if (length(spotStep)>1) stop("Only a scalar string allowed for spotStep");
	if (length(tdm$umode)>1) stop("Only a scalar string allowed for tdm$umode");
	if (length(indVec)==1 & tdm$parallelCPUs>1) {
	  warning("There is only one job (length(indVec)==1) --> parallelization would not run, so we set tdm$ParallelCPUs=1");
	  tdm$parallelCPUs=1;
  }

  if (tdm$parallelCPUs>1) {
    cl = prepareParallelExec(tdm);
    sappResult <- parallel::parSapply(cl, indVec, tuneItStep, theTuner,expeVec,confFile,spotStep,dataObj,envT,tdm);
    parallel::stopCluster(cl);
  } else {  
		sappResult <- sapply(indVec, tuneItStep, theTuner,expeVec,confFile,spotStep,dataObj,envT,tdm);
  }
  # populate envT with the results returned in matrix sappResult:
	envT <- populateEnvT(sappResult,envT,tdm,spotStep);
  
  if (TRUE) { #(spotStep == "auto") {
    saveEnvT(envT,tdm$runList,saveModel=tdm$U.saveModel);
    saveSRFinfo(envT);
  } 

  if (nrow(envT$theFinals)>0) {
    print(envT$theFinals);
  } else {
    cat("Note: No rows in data frame envT$theFinals\n");
  }

  envT;
} # function tdmTuneIt

      ######################################################################################
  		#------------------------------------------------------------------------------------------------------
      #  tuneItStep: helper function for tdmTuneIt, called via sapply or parSapply:
      #  
      tuneItStep <- function(ind,theTuner,expeVec,confFile,spotStep,dataObj,envT,tdm) {
        if (tdm$parallelCPUs>1) library(TDMR);
        nExp = expeVec[ind];
        nConf = which(confFile==envT$runList);
        
        print(c(ind,confFile,nExp,theTuner));
        envT$spotConfig <- sC <- envT$sCList[[nConf]]; # spotGetOptions(srcPath=tdm$theSpotPath,confFile);
        envT$theTuner <- theTuner;
        envT$nExp <- envT$spotConfig$nExp <- nExp;
        envT$bst <- NULL;
        envT$res <- NULL; 
        if (spotStep=="rep" | spotStep=="report" ) {
          envT$bst = envT$getBst(envT,confFile,nExp,theTuner);
          envT$res = envT$getRes(envT,confFile,nExp,theTuner);
        }

        if (tdm$fileMode) {  
          if (is.null(tdm$finalFile)) tdm$finalFile = sub(".conf",".fin",confFile);
          tFinalFile <- ifelse(tdm$nExperim>1, sprintf("%s-e%02d%s",sub(".fin","",tdm$finalFile,fixed=TRUE),nExp,".fin"), tdm$finalFile);
          # i.e. if tdm$finalFile="cpu.fin", then tFinalFile="cpu-e02.fin" for nExp=2
          if (file.exists(tFinalFile)) file.remove(tFinalFile);
        } 
        # NEW 09/2012: always operate SPOT with spot.fileMode=FALSE (take everything from  envT$spotConfig)
        envT$spotConfig$spot.fileMode=FALSE;
        
        #
        # data are already read in into dataObj
        #
        envT$spotConfig$opts$TST.COL = dataObj$TST.COL;    # this column has to be subtracted in main_* from the input variables

        ptm <- proc.time();
            # this is for the case spotStep=="rep":
            envT$spotConfig$alg.currentResult <- envT$res;
            envT$spotConfig$alg.currentBest <- envT$bst;	
            writeLines(sprintf("*** Starting TUNER %s, spotStep=%s, on task %s ***\n",theTuner,spotStep,confFile),con=stderr());
            tdmDispatchTuner(theTuner,confFile,spotStep,tdm,envT,dataObj);
            # If spotStep=="auto" then tdmDispatchTuner runs the tuning process, puts its results in envT$bst, envT$res 
            # and returns the (extended) spotConfig in envT$tunerVal.
            # If spotStep=="rep" then tdmDispatchTuner expects that envT$bst, envT$res contain already the right data frames 
            # and it runs just the reporting step of SPOT.
        
        #
        # now we have envT$bst and envT$res filled in any case (either from tdmDispatchTuner or from a previous setting)
        #
        if (is.null(envT$bst)) stop("No contents for data frame envT$bst");
        if (is.null(envT$res)) stop("No contents for data frame envT$res");
        
        if (is.null(tdm$timeMode)) stop("tdm$timeMode is not set (NULL). Consider 'tdm <- tdmDefaultsFill(tdm)' to set all defaults");
        time.txt = c("Proc", "System", "Elapsed");
        envT$time.TRN=(proc.time()-ptm)[tdm$timeMode]; opts2=list(); opts2$VERBOSE=1;  
        cat1(opts2,paste(time.txt[tdm$timeMode], "time for tuning with tdmDispatchTuner:",envT$time.TRN,"sec\n"));     

        ptm <- proc.time();
        finals <- NULL;
        if (tdm$nrun>0) {
          writeLines(paste("*** starting",tdm$unbiasedFunc,"for",confFile,"with umode=",tdm$umode,"***\n"),con=stderr());
          cmd=paste("finals <-",tdm$unbiasedFunc,"(confFile,envT,dataObj,umode=tdm$umode,finals=finals,withParams=envT$wP,tdm=tdm)",sep="");
          eval(parse(text=cmd));
          time.TST=(proc.time()-ptm)[tdm$timeMode];   
          cat1(opts2,paste(time.txt[tdm$timeMode], "time for",tdm$unbiasedFunc,":",time.TST,"sec\n"));   
          finals <- cbind(finals,Time.TST=time.TST);  

          # transport back opts$srf (data frame with SRF info in case opts$SRF.calc==TRUE)
          envT$sCList[[nConf]]$opts$srf = envT$result$lastRes$opts$srf

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
        # parSapply (parallel execution) can not deal with environments as return values.
        list( tunerVal=envT$tunerVal    # last tuning result: spotConfig with extensions       
             ,bst=envT$bst              # last tuning result       
             ,res=envT$res              # last tuning result  
             ,result=envT$result        # last tuning result
             ,roi=envT$tunerVal$alg.roi      
             ,theFinals=finals   ###tail(envT$theFinals,1)
             );      
  		} # end of function tuneItStep
  		#------------------------------------------------------------------------------------------------------

######################################################################################
# the helper functions for tdmTuneIt 
#     populateEnvT, saveEnvT, saveSRFinfo, removeTmpfiles2, prepareParallelExec
# are in tdmBigLoop.r
######################################################################################
