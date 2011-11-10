######################################################################################
# helper fct for tdmCompleteEval in case tdm$parallelCPUs>1: 
#     Populate the global envT after parallel execution  with snowfall (sfSapply with results in sappResult).
#     For simplicity we use it also after sequential execution (sapply with results in sappResult). 
populateEnvT <- function(sappResult,envT,tdm) {
      for (ind in 1:(tdm$nExperim*envT$nTuner*envT$nRunList)) {
          envT$nGrid = envT$nGrid+1;
          envT$bstGrid[[envT$nGrid]] <- as.data.frame(sappResult["bst",ind][[1]]);
          envT$resGrid[[envT$nGrid]] <- as.data.frame(sappResult["res",ind][[1]]);   
          envT$theFinals <- rbind(envT$theFinals,as.data.frame(sappResult["theFinals",ind][[1]]));
          # "[[1]]" is necessary to avoid prefix "theFinals." in the header names 
      }
      rownames(envT$theFinals) <- (1:nrow(envT$theFinals));
      envT$bst <- as.data.frame(sappResult["bst",ncol(sappResult)][[1]]);
      envT$res <- as.data.frame(sappResult["res",ncol(sappResult)][[1]]);
}

######################################################################################
# helper fct for tdmCompleteEval: 
#     Save a small version of environment envT (passed in as thisEnvT) on runList[1].rda
saveEnvT <- function(thisEnvT,runList) {
      envT = list() # new.env(); #            # when saving thisEnvT, we copy the relevant elements to a *list* envT
                                              # and skip the function elements (getBst,getInd,getRes) in envT, because 
                                              # they would also save *their* environment which tends to make the .rda file rather big
      for (ele in setdiff(ls(thisEnvT),c("getBst","getInd","getRes"))[9:9]) 
        eval(parse(text=paste("print(ele); envT$",ele," = thisEnvT$",ele,sep="")));
        
                                              # for the save on .rda file we delete also some potentially voluminous elements 
      envT$result$last_res$d_train=NULL;      # from the local envT, we want only res and bst objects + describing vars
      envT$result$last_res$d_test=NULL;
      envT$result$last_res$d_dis=NULL;
      envT$result$last_res$last.probs=NULL;
      envT$result$last_res$last.rf=NULL;
      envT$result$dset=NULL;
      save(envT,file=sub(".conf",".rda",runList[1],fixed=TRUE));              
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
        if (wP==FALSE) warning("The design parameters in differents ROI files are different --> TDM sets tdm$withParams to FALSE (no param columns in envT$theFinals)");      
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
   
######################################################################################
# tdmCompleteEval:
#
#' Tuning and unbiased evaluation in a big loop.
#' For each \code{.conf} file in \code{runList} do tuning (SPOT, CMA-ES or other) and one or several 
#' runs of \code{tdm$unbiasedFunc} (as many as \code{tdm$umode} has elements).
#'
#' Tuning is skipped if the \code{.conf} file does not appear in \code{spotList} or if \code{!(spotStep=="auto")}. In this
#' case it is assumed then that the appropriate \code{.bst} and \code{.res} files exist already.
#'
#'  @param runList   vector of \code{.conf} filenames
#'  @param spotList  \code{[NULL]} vector of \code{.conf} filenames for which spot tuning is done.
#'                   If \code{NULL}, then \code{spotList=runList}.
#'  @param spotStep  \code{["auto"]} which step of SPOT to execute (either \code{"auto"} or \code{"rep"}). Entries in this vector are
#'                   cyclically recycled if \code{spotStep} is shorter than \code{runList}.
#'  @param tdm      a list from which we need here the elements
#'     \describe{
#'     \item{\code{mainCommand}}{ the command to be called for unbiased evaluations}
#'     \item{\code{mainFile}}{ change to the directory of mainFile before starting mainCommand}
#'     \item{\code{tuneMethod}}{vector of tuning method(s) \code{"spot", "cmaes", "lhd", "bfgs", "powell"}}
#'     \item{\code{unbiasedFunc}}{the function for unbiased evaluations to call}
#'     \item{\code{umode}}{a vector of strings containing the unbiased resampling strategies
#'                         to execute \code{"RSUB", "TST", "CV"}, see \code{mapOpts} in tdmMapDesign.}
#'     \item{\code{finalFile}}{filename where to save \code{envT$theFinals}}
#'     \item{\code{withParams}}{\code{[NULL]} If \code{=TRUE}: include best parameters as columns in output \code{envT$theFinals}.  
#'			    If \code{=FALSE}: don't (this is appropriate if \code{runList} combines several .conf files which differ in their parameters)}
#'     }
#'
#'   @return environment envT, containing
#'      \item{res}{ data frame with results from last tuning (one line for each call of \code{tdmStart*})} 
#'      \item{bst}{ data frame with the best-so-far results from last tuning (one line collected after each (SPO) step)}
#'      \item{theFinals}{ data frame with one-line result for each triple \code{confFile,nExp,tuner}:
#'            \preformatted{confFile tuner nExp [params] RGain.bst RGain.CV sdG.CV RGain.TST sdG.TST }
#'                    where each 'sdG.' denotes the standard deviation of the preceeding RGain
#'                    and where \code{[params]} is written depending on \code{tdm$withParams}.
#'                    \code{RGain} denotes the relative gain on a certain data set: the actual gain
#'                    achieved with the model divided by the maximum gain possible for the 
#'                    current cost matrix and the current data set.}
#'      \item{result}{ list with results of \code{tdm$mainCommand} as called in the last unbiased evaluation}
#' @note Side Effects:
#'   If \code{tdm$fileMode==TRUE}, several files are written in the directory of the \code{.conf} file:
#'     \itemize{
#'         \item \code{envT } is saved to \code{<runList[1]>.rda} file
#'         \item \code{envT$theFinals } is written to file \code{tdm$finalFile} and appended to \code{tdm$experFile}
#'         \item \code{envT$res } is written to \code{.res} file in directory \code{<tuneMethod>}
#'         \item \code{envT$bst } is written to \code{.bst} file in directory \code{<tuneMethod>}
#'      }
#'   More precisely: If we make tuning with \code{tuneMethod="lhd"} and perform the 3rd experiment for \code{.conf} file
#'   \code{cpu_01.conf}, then the \code{.res} file is written to \code{lhd/cpu_01_lhd_03.res} relative to the directory 
#'   of \code{.conf} file. Analoguously for \code{.bst} file.
#' 
#' An example usage of function tdmCompleteEval is shown in demo/demo01cpu.r, accessible via \code{demo(demo01cpu)} 
#'
#' @author Wolfgang Konen, Patrick Koch
#' @export
######################################################################################
tdmCompleteEval <- function(runList,spotList=NULL,spotStep="auto",tdm) {
  tdm <- tdmDefaultsFill(tdm);
  if (is.null(tdm$finalFile)) tdm$finalFile <- sub(".conf",".fin",runList[1],fixed=TRUE);
  if (is.null(spotList)) spotList <- runList;
  wP <- ifelse(is.null(tdm$withParams),ifelse(length(runList)==1,TRUE,FALSE),tdm$withParams)

  envT <- new.env();      # The environment envT is passed by reference into the inner functions
                          # which means that it can be used a) to transport information back from
                          # those inner functions and b) to transport information to and back even 
                          # for functions like tdmStartOther which are not allowed to have envT in 
                          # their argument list
                          
  envT$nGrid <- 0;
  envT$nTuner <- length(tdm$tuneMethod);
  envT$nRunList <- length(runList);
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
    ind = indTuner + envT$nTuner*((nExp-1) + tdm$nExperim*(nConf-1));
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
  
  #
  # do all necessary file reading **before**  branching into completeEvalConfNexpTuner
  # (completeEvalConfNexpTuner can be in parallel execution branch, where file access might be not possible)
  #
  k=0;
  for (confFile in runList) {
    k=k+1;
    if (!file.exists(confFile)) stop(sprintf("Could not find confFile=%s in current dir=%s",confFile,getwd()));
    envT$sCList[[k]] <- spotGetOptions(srcPath=tdm$theSpotPath,confFile);
    sC <- envT$sCList[[k]]; print(sC$spot.seed); 
    wP <- checkRoiParams(wP,confFile,sC,envT);
    pdFile = sC$io.apdFileName;
  	print(pdFile);	  
    if (!file.exists(pdFile)) stop(sprintf("Could not find apdFile=%s in current dir=%s",pdFile,getwd()));
    opts <- NULL;     # just to make 'R CMD check' happy  (and in case that after sourcing pdFile 'opts' is not there as expected)
  	source(pdFile,local=TRUE);        # read problem design  (here: all elements of list opts)   
		if (is.null(opts))
		  warning(sprintf("%s does not define the required object opts.",opts));  	
 	  envT$sCList[[k]]$opts=opts;
  }
  tdmMapDesLoad(envT,tdm); 
  tdmMapDesSpot$load(tdm); 
  source(tdm$mainFile);
  mainFunc <- sub(".R","",sub(".r","",basename(tdm$mainFile),fixed=TRUE));   # e.g. "main_cpu"
  if (tdm$parallelCPUs>1) sfExport(list=c("tdmMapDes",mainFunc));
  
  		#------------------------------------------------------------------------------------------------------
      #  Introduce a new function suitable for sapply or sfSapply:
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
          if (file.exists(tFinalFile) & theTuner==tuneVec[1] & confFile==runList[1]) file.remove(tFinalFile);
        } 
       
        # make temporary files so that parallel runs will not interfere: 
        appendix <- sprintf("%s_%02d",theTuner,nExp);
        tConfFile <- copyToTmpfile(confFile,confFile,".conf",appendix);
        tRoiFile <- copyToTmpfile(confFile,sC$io.roiFileName,".roi",appendix);                                        
        #tApdFile <- copyToTmpfile(confFile,sC$io.apdFileName,".apd",appendix);                                        
        sC <- spotGetOptions(srcPath=tdm$theSpotPath,tConfFile);
              # this configures sC$io.bstFileName and sC$io.resFileName with the right endings 
              # (matching to tConfFile); therefore confFile itself should NOT define these filenames (!)
        sC$io.roiFileName <- tRoiFile;
        #sC$io.apdFileName <- tApdFile;            # not needed any longer
        sC$opts <- envT$sCList[[nConf]]$opts;
        envT$spotConfig <- sC;
 
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
              tdmDispatchTuner(theTuner,tConfFile,spotStep[i],tdm,envT);
              # tdmDispatchTuner puts its results in envT$bst and envT$res.
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
              tdmDispatchTuner(theTuner,tConfFile,spotStep[i],tdm,envT);
              # tdmDispatchTuner puts its results in envT$bst and envT$res
             
        } # if(tdm$fileMode)        
        #
        # now we have envT$bst and envT$res filled in any case (either from tdmDispatchTuner or from file)
        #
        
        #--- obsolete, this is now done by populateEnvT (also for tdm$parallelCPUs==1) ---
        #        if (tdm$parallelCPUs==1) {
        #          envT$nGrid = envT$nGrid+1;
        #          envT$bstGrid[[envT$nGrid]] <- envT$bst;
        #          envT$resGrid[[envT$nGrid]] <- envT$res;
        #        }
        time.TRN=(proc.time()-ptm)[tdm$timeMode]; opts=list(); opts$VERBOSE=1;  
        time.txt = c("Proc", "", "Elapsed");
        cat1(opts,paste(time.txt[tdm$timeMode], "time for tuning with tdmDispatchTuner:",time.TRN,"sec\n"));     
        
        ptm <- proc.time();
        finals <- NULL;
        for (umode in tdm$umode) {
          cat("*** starting",tdm$unbiasedFunc,"for",tConfFile,"with umode=",umode,"***\n");
          cmd=paste("finals <-",tdm$unbiasedFunc,"(tConfFile,envT,umode=umode,finals=finals,withParams=wP,tdm=tdm)",sep="");
          eval(parse(text=cmd));
        }
        time.TST=(proc.time()-ptm)[tdm$timeMode];   
        cat1(opts,paste(time.txt[tdm$timeMode], "Elapsed time for",tdm$unbiasedFunc,":",time.TST,"sec\n"));   

        finals <- cbind(finals,Time.TST=time.TST,Time.TRN=time.TRN);  
        
        #--- obsolete, this is now done by populateEnvT (also for tdm$parallelCPUs==1) ---
        #        envT$theFinals <- rbind(envT$theFinals,finals);
        
        if (tdm$fileMode) {  
          removeTmpfiles(tConfFile);
      		colNames = ifelse(file.exists(tFinalFile),FALSE,TRUE);
      		write.table(finals
      				, file = tFinalFile
      				, col.names= colNames
      				, row.name = FALSE
      				, append = !colNames
      				, sep = " ",
      				, quote = FALSE
      				, eol = "\n"
      		);
      		if (!is.null(tdm$experFile)) {
        		colNames = ifelse(file.exists(tdm$experFile),FALSE,TRUE);
        		write.table(finals             # multiple execution of same experiment (nExperim>1 in script_all.R)
        				, file = tdm$experFile
        				, col.names= colNames
        				, row.name = FALSE
        				, append = !colNames
        				, sep = " ",
        				, quote = FALSE
        				, eol = "\n"
        		);
      		}
        } # if(tdm$fileMode)
        
        # we return a *list* with the most important elements from environment envT and not envT itself, because 
        # sfSapply (parallel execution) can not deal with environments as return values.
        list( bst=envT$bst              # last tuning result       
             ,res=envT$res              # last tuning result        
             ,theFinals=finals   ###tail(envT$theFinals,1)
             );      
  		} # end of function completeEvalConfNexpTuner
  		#------------------------------------------------------------------------------------------------------
  		
	#
	# depending on tdm$parallelCPUs, the following lines execute the code either in parallel 
	# or sequential: For each value of indVec the function completeEvalConfNexpTuner is 
	# called. The vectors tuneVec, expeVec, confVec contain for each element of indVec the
	# corresponding value of tuner, experiment number and .conf file, resp.
	#
	indVec <- 1:(tdm$nExperim*envT$nTuner*envT$nRunList);
	tuneVec <- rep(tdm$tuneMethod,tdm$nExperim*envT$nRunList);
	expeVec <- rep(sort(rep(1:tdm$nExperim,envT$nTuner)),envT$nRunList);
	confVec <- sort(rep(runList,tdm$nExperim*envT$nTuner));
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

  saveEnvT(envT,runList);
  
  print(envT$theFinals);

  envT;
} # function tdmCompleteEval


