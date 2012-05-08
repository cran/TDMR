#-------- MIES part temporarily moved to ../tdmDispatch_miesPart.r

###########################################################################################
#tdmDispatchTuner:
#
#'     Helper function for \code{\link{tdmCompleteEval}}.
#'
#'     tdmDispatchTuner selects and starts the tuner specified by tuneMethod. \cr
#'     See the 'Details' section of \code{\link{tdmCompleteEval}} for a list of available tuners
#'
#' @param tuneMethod the tuning algorithm given as a string. Possible values are \{ "spot" | "lhd" | "cmaes" | "cma_j" | "bfgs" | "powell" \}.
#' @param confFile the configuration file.
#' @param spotStep which step to execute for \link{SPOT} . Values "rep" and "auto" are supported by TDMR.
#' @param tdm the TDMR object
#' @param envT the environment variable
#' @param dataObj the \code{\link{TDMdata}} object containing the data set (train/vali part and test part)
#'
#' @return the result of the tuning algorithm.
#' @seealso   \code{\link{tdmCompleteEval}}
#' @author Wolfgang Konen, FHK, Sep'2010 - Oct'2011
#' @export
#' @keywords internal
###########################################################################################
tdmDispatchTuner <- function(tuneMethod,confFile,spotStep,tdm,envT,dataObj)
{   theSpotPath=tdm$theSpotPath
    if (spotStep=="auto") {
      #if (is.null(tdm$mainFile)) stop("Element tdm$mainFile is missing, but this is required for spotStep='auto'");
      if (is.null(tdm$mainFunc)) stop("Element tdm$mainFunc is missing, but this is required for spotStep='auto'");
    }
    
    tunerVal = switch(spotStep
      ,"rep" = spot(confFile,spotStep,theSpotPath)
      ,"auto" = switch(tuneMethod
                ,"spot" = spotTuner(confFile,spotStep,theSpotPath,tdm,envT,dataObj)
                ,"lhd" = lhdTuner(confFile,spotStep,theSpotPath,tdm,envT,dataObj) 
                ,"cmaes" = cmaesTuner(confFile,tdm,envT,dataObj)
                #,"mies" = miesTuner(confFile,tdm,envT,dataObj)  
                ,"cma_j" = cma_jTuner(confFile,tdm,envT,dataObj) 
                ,"bfgs" = bfgsTuner(confFile,tdm,envT,dataObj) 
                ,"powell" = powellTuner(confFile,tdm,envT,dataObj) 
                ,"INVALID1"
                )
      ,"INVALID2"
      );

    if (tunerVal[1]=="INVALID1") {
      stop(sprintf("*** Invalid tuneMethod=%s ***\n",tuneMethod));
    }
    if (tunerVal[1]=="INVALID2") {
      stop(sprintf("*** Invalid spotStep=%s ***\n",spotStep));
    }
    envT$tunerVal = tunerVal;
    envT$tunerVal$dataObj = NULL;     # delete this potential voluminous element
    
    tunerVal;
}

######################################################################################
#' Perform a \link{SPOT} tuning. 
#'
#' \code{spotTuner} calls \code{\link{spot}} and write the necessary information on environment \code{envT}.
#'
#' @param confFile task configuration for tuning algorithm
#' @param spotStep needed for spot
#' @param theSpotPath needed for spot
#' @param tdm the TDMR object
#' @param envT the environment variable
#' @param dataObj the \code{\link{TDMdata}} object containing the data set (train/vali part and test part)
#'
#' @return the result of SPOT tuning, i.e. the list \code{spotConfig}
#'
# @rdname  tdmTuners
# @aliases lhdTuner spotTuner
#'
#' @export
#' @keywords internal
######################################################################################
spotTuner <- function(confFile,spotStep,theSpotPath,tdm,envT,dataObj)
{
    if (tdm$fileMode==FALSE) envT$spotConfig$spot.fileMode=FALSE;
    
    sC <- envT$spotConfig;
    #sC$alg.func <- "tdmStartSpot";
    #sC$spot.fileMode = TRUE; 
    sC$tdm <- tdm;              # needed for tdm$mainFile, tdm$mainFunc, tdm$fileMode
    sC$alg.currentResult <- NULL;
    sC$dataObj <- dataObj;

    sC <- spot(confFile,spotStep,theSpotPath,sC);
    # spot calls tdmStartSpot. 
    # tdmStartSpot reads opts from sC$opts, dataObj from sC$dataObj, and writes sC$alg.currentResult

    envT$res <- sC$alg.currentResult;
    envT$bst <- sC$alg.currentBest;	
    sC;
}

######################################################################################
#' Perform LHD tuning. 
#'
#' Perform a parameter tuning using a Latin hypercube design (LHD) 
#' for obtaining best design points. LHD is performed by configuring SPOT 
#' in such a way that all the budget is used for the initial design (usually LHD).
#' 
#' @param confFile task configuration for tuning algorithm
#' @param spotStep needed for spot
#' @param theSpotPath needed for spot
#' @param tdm the TDMR object
#' @param envT the environment variable
#' @param dataObj the \code{\link{TDMdata}} object containing the data set (train/vali part and test part)
#'
#' @return the result of LHD sampling, i.e. the list \code{spotConfig}
#'
#' @export
#' @keywords internal
######################################################################################
lhdTuner <- function(confFile,spotStep,theSpotPath,tdm,envT,dataObj)
{
    if (tdm$fileMode==FALSE) envT$spotConfig$spot.fileMode=FALSE;

    sC <- envT$spotConfig;
    sC$tdm <- tdm;              # needed for tdm$mainFile, tdm$mainFunc, tdm$fileMode
    sC$alg.currentResult <- NULL;

    fncall1 = sC$auto.loop.nevals;
    fncall2 = sC$auto.loop.steps* sC$seq.design.new.size* sC$seq.design.maxRepeats +
              sC$init.design.size* sC$init.design.repeats  +
             (sC$seq.design.maxRepeats- sC$init.design.repeats);
    new.nevals = min(fncall1,fncall2); 
    #sC$alg.func = "tdmStartSpot";
    sC$auto.loop.steps = 0;
    sC$init.design.size = round(new.nevals/sC$seq.design.maxRepeats);
    sC$init.design.repeats = sC$seq.design.maxRepeats;
    sC$dataObj <- dataObj;

    sC <- spot(confFile,spotStep,theSpotPath,sC);
    # spot calls tdmStartSpot. 
    # tdmStartSpot reads opts from sC$opts, dataObj from sC$dataObj, and writes sC$alg.currentResult

    envT$res <- sC$alg.currentResult;
    envT$bst <- sC$alg.currentBest;	
    sC;
}


######################################################################################
#' Perform CMA-ES tuning (R version). 
#'
#' Perform a parameter tuning by CMA-ES, using the *R*-implementation 
#' (package \code{cmaes} by Olaf Mersmann).
#' 
#' @param confFile task configuration for tuning algorithm
#' @param tdm the TDMR object
#' @param envT the environment variable
#' @param dataObj the \code{\link{TDMdata}} object containing the data set (train/vali part and test part)
#'
#' @return result of CMA-ES algorithm
#' @export
#' @keywords internal
######################################################################################
cmaesTuner <- function(confFile,tdm,envT,dataObj)
{
    require(cmaes)
    envT$spotConfig$alg.currentResult <- NULL;    
    sC <- envT$spotConfig;
    roiLower <- sC$alg.roi[,1];
    roiUpper <- sC$alg.roi[,2];
    set.seed(sC$spot.seed);
    param <- runif(length(roiLower),roiLower,roiUpper);#/2;    # start configuration
    if (tdm$fileMode) {
      tdm$resFile <-  sC$io.resFileName;   
      tdm$bstFile <-  sC$io.bstFileName;   
      if (file.exists(sC$io.resFileName)) file.remove(sC$io.resFileName);
      if (file.exists(sC$io.bstFileName)) file.remove(sC$io.bstFileName);
    }
    
    if(is.null(tdm$startOtherFunc)) tdmStartOther = makeTdmStartOther(tdm,envT,dataObj) else
      tdmStartOther = tdm$startOtherFunc;


    fncall1 = sC$auto.loop.nevals;
    fncall2 = sC$auto.loop.steps* sC$seq.design.new.size* sC$seq.design.maxRepeats 
            + sC$init.design.size* sC$init.design.repeats;
            + (sC$seq.design.maxRepeats- sC$init.design.repeats);
    control = list(mu=4,lambda=4);
    control$maxit=min(fncall1,fncall2)/sC$seq.design.maxRepeats/control$lambda;
    # Be aware, that control$lambda (# of offsprings) controls the # of fct calls, 
    # NOT control$mu. The number of calls to tdmStartOther will be N=ceiling(control$maxit)*control$lambda, 
    # the number of calls to the DM training fct main_TASK will be N*sC$seq.design.maxRepeats.
    control$diag.pop=FALSE;
    control$diag.sigma=TRUE;
    control$diag.eigen=TRUE;
    
    cmaVal <- cma_es(param,tdmStartOther,sC$opts,lower=roiLower,upper=roiUpper,control=control);
    # cma_es calls tdmStartOther and tdmStartOther appends to data frames envT$res and envT$bst.
    # If tdm$fileMode==TRUE, then tdmStartOther will write envT$res to envT$spotConfig$io.resFileName
    # and envT$bst to envT$spotConfig$io.bstFileName 
    
    tunerVal = sC;
    tunerVal$cma = cmaVal;
    #tunerVal$cma$sres <- sres; 
    tunerVal$cma$count=nrow(envT$res);
    
    cat(sprintf("Function calls to tdmStartOther: %d\n",tunerVal$cma$count[1]));

    tunerVal;
}


######################################################################################
# cma_jTuner
#
#' Perform CMA-ES tuning (Java version). 
#' 
#' Perform a parameter tuning by CMA-ES, using the *Java* 
#' implementation by Niko Hansen.
#' 
#' @param confFile task configuration for tuning algorithm
#' @param tdm the TDMR object
#' @param envT the environment variable
#' @param dataObj the \code{\link{TDMdata}} object containing the data set (train/vali part and test part)
#'
#' @return result of CMA-ES algorithm
#' @export
#' @keywords internal
######################################################################################
cma_jTuner <- function(confFile,tdm,envT,dataObj)
{
    envT$spotConfig$alg.currentResult <- NULL;
    if (tdm$fileMode==FALSE) {
      envT$spotConfig$spot.fileMode=FALSE;
    } else {
      cat("NOTE: It is recommended to use cma_jTuner with the setting tdm$fileMode=FALSE. Otherwise it might not work properly on some OS.");
    }
    sC <- envT$spotConfig;
    roiLower <- sC$alg.roi[,1];
    roiUpper <- sC$alg.roi[,2];
    set.seed(sC$spot.seed);
    param <- runif(length(roiLower),roiLower,roiUpper);#/2;    # start configuration

    if (tdm$fileMode) {
      tdm$resFile <-  sC$io.resFileName;   
      tdm$bstFile <-  sC$io.bstFileName;   
      if (file.exists(sC$io.resFileName)) file.remove(sC$io.resFileName);
      if (file.exists(sC$io.bstFileName)) file.remove(sC$io.bstFileName);
    }
    
    if(is.null(tdm$startOtherFunc)) tdmStartOther = makeTdmStartOther(tdm,envT,dataObj) else
      tdmStartOther = tdm$startOtherFunc;

    repeats = min(sC$auto.loop.steps+1, sC$seq.design.maxRepeats);
    K = max(0,repeats-(sC$init.design.repeats+1));
    fncall1 = sC$auto.loop.nevals;
    fncall2 = sC$auto.loop.steps* sC$seq.design.new.size* repeats + 
              sC$init.design.size* sC$init.design.repeats -
              (sC$seq.design.new.size-1)*K*(K+1)/2;
    fncall = min(fncall1,fncall2);              

    #--- this is the normal version for javadir ---
    if (is.null(tdm$tdmPath)) {
        javadir=paste(.find.package("TDMR"),"javabin",sep="/");
    } else {
        javadir=paste(tdm$tdmPath,"inst","javabin",sep="/");
    }
    #--- next line is for the Eclipse developer version only ---
    #javadir="C:/Dokumente und Einstellungen/wolfgang/Eigene Dateien/ProjectsWK/ReinforceLearn/cmaes_java/cma/bin"
    
		oldWD = getwd(); setwd(javadir);        # save & change working dir
    save.image(file="cma_j1.rda")                      # all function objects def'd in .GlobalEnv 
    save(list=ls(all.names=TRUE),file="cma_j2.rda")    # envT, tdm, tdmStartOther and other local variables
   
		write.table(t(c("dimension",length(roiLower))), file="CMAprops.txt"
                , quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE);
    s="initialX ="; for (i in 1:length(param)) s=paste(s,param[i]);                
		write.table(s, file="CMAprops.txt", quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE, append=TRUE)
    s="initialStandardDeviations ="; for (i in 1:length(roiUpper)) s=paste(s,(roiUpper[i]-roiLower[i])/2);
		write.table(s, file="CMAprops.txt", quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE, append=TRUE)
    s="lowerBounds ="; for (i in 1:length(roiLower)) s=paste(s,roiLower[i]);
		write.table(s, file="CMAprops.txt", quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE, append=TRUE)
    s="upperBounds ="; for (i in 1:length(roiUpper)) s=paste(s,roiUpper[i]);
		write.table(s, file="CMAprops.txt", quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE, append=TRUE)
		write.table(t(c("stopMaxFunEvals",fncall/sC$seq.design.maxRepeats)), file="CMAprops.txt"
                , quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE, append=TRUE)
		callString = paste("java cma.examples.cmaJava CMAprops.txt");
		#print(callString);
		#browser()
	
		sres <- system(callString, intern= TRUE);
		if (length(grep("cma_j.r returned with error status",sres))>0) {
		  print(sprintf("%s",sres));
		  stop("Error in cma_jTuner: cma_j.r returned with error status >> check cma_j.err.");
    }
    # cmaJava calls repeatedly "R cma_j.r" which reloads cma_j1.rda, cma_j2.rda and then executes tdmStartOther.
    # (Be aware that on Windows the path to R.exe must be in the system variable Path.)
    # tdmStartOther appends to data frames envT$res and envT$bst.
    # Deprecated: If tdm$fileMode==TRUE, then tdmStartOther will write envT$res to envT$spotConfig$io.resFileName
    # and envT$bst to envT$spotConfig$io.bstFileName.
    # Finally cma_j.r will save cma_j3.rda (the modified elements res,bst,spotConfig of envT, but not envT itself) 
    
    res <- bst <- spotConfig <- NULL;     # just to make 'R CMD check' happy  (and in case that cma_j3.rda has not the content as expected)
		load("cma_j3.rda");         # load the things modified in envT    (spotConfig, res, bst, as saved by javabin/cma_j.r)
		if (is.null(res) | is.null(bst) | is.null(spotConfig))
		  warning(sprintf("%s/cma_j3.rda does not contain all the required objects: res, bst, spotConfig.",javadir));
		envT$res = res;
		envT$bst = bst;
		envT$spotConfig = spotConfig;
		
		setwd(oldWD);
		
    tunerVal = sC;
    tunerVal$cma = list()
    tunerVal$cma$sres <- sres; 
    tunerVal$cma$count=nrow(envT$res);
    
    cat(sprintf("Function calls to tdmStartOther: %d\n",tunerVal$cma$count[1]));

    tunerVal;
}



######################################################################################
#' Perform Powell's tuning. 
#'
#' Perform a parameter tuning by Powell's UObyQA algorithm 
#' (unconstrained optimization by quadratic approximation), see R-package \code{powell}
#' 
#' @param confFile task configuration for tuning algorithm
#' @param tdm the TDMR object
#' @param envT the environment variable
#' @param dataObj the \code{\link{TDMdata}} object containing the data set (train/vali part and test part)
#'
#' @return result of Powell's algorithm
#' @export
#' @keywords internal
######################################################################################
powellTuner <- function(confFile,tdm,envT,dataObj){
    require(powell)
  
    #function to fix constraint violating steps:
    tdm$constraintFnc <- function(x,tdm) {
      for(i in 1:length(x)){
        if(x[i] < envT$spotConfig$alg.roi[i,1]) {x[i] <- envT$spotConfig$alg.roi[i,1]}
        if(x[i] > envT$spotConfig$alg.roi[i,2]) {x[i] <- envT$spotConfig$alg.roi[i,2]}
      }
      x;
    }
    
    if (tdm$fileMode==FALSE) envT$spotConfig$spot.fileMode=FALSE;
    envT$spotConfig$alg.currentResult <- NULL;
 
    sC <- envT$spotConfig;
    roiLower <- sC$alg.roi[,1];
    roiUpper <- sC$alg.roi[,2];
  
    if (tdm$fileMode) {
      tdm$resFile <-  sC$io.resFileName;   
      tdm$bstFile <-  sC$io.bstFileName;   
      if (file.exists(sC$io.resFileName)) file.remove(sC$io.resFileName);
      if (file.exists(sC$io.bstFileName)) file.remove(sC$io.bstFileName);
    }
  
    if(is.null(tdm$startOtherFunc)) tdmStartOther = makeTdmStartOther(tdm,envT,dataObj) else
      tdmStartOther = tdm$startOtherFunc;

    param <- (roiLower+roiUpper)/2;    # start configuration
  
    maxEvaluations = sC$auto.loop.nevals/sC$seq.design.maxRepeats;
#browser()  
    tunerVal <- powell(param, fn=tdmStartOther, control=list(maxit=maxEvaluations), check.hessian=FALSE, opts=sC$opts);
    # powell calls tdmStartOther and tdmStartOther writes envT$res and envT$bst

    tunerVal;  
}


######################################################################################
#' Perform BFGS tuning. 
#'
#' Perform a parameter tuning by Broyden, Fletcher, Goldfarb and Shanno (BFGS) method.
#' The L-BFGS-B version allowing box constraints is used.
#' 
#' @param confFile task configuration for tuning algorithm
#' @param tdm the TDMR object
#' @param envT the environment variable
#' @param dataObj the \code{\link{TDMdata}} object containing the data set (train/vali part and test part)
#'
#' @return result of BFGS algorithm
#' @author Wolfgang Konen, Patrick Koch \email{wolfgang.konen@@fh-koeln.de}
#' @export
#' @keywords internal
######################################################################################
bfgsTuner <- function(confFile,tdm,envT,dataObj){
    #require("optimx")
    envT$spotConfig$alg.currentResult <- NULL;
    if (tdm$fileMode==FALSE) envT$spotConfig$spot.fileMode=FALSE;
    sC <- envT$spotConfig;
    roiLower <- sC$alg.roi[,1];
    roiUpper <- sC$alg.roi[,2];
    param <- runif(length(roiLower), min=roiLower, max=roiUpper) # create start vector generating uniformly randomized vector
     
    if (tdm$fileMode) {
      tdm$resFile <-  sC$io.resFileName;   
      tdm$bstFile <-  sC$io.bstFileName;   
      if (file.exists(sC$io.resFileName)) file.remove(sC$io.resFileName);
      if (file.exists(sC$io.bstFileName)) file.remove(sC$io.bstFileName);
    }
  
    if(is.null(tdm$startOtherFunc)) tdmStartOther = makeTdmStartOther(tdm,envT,dataObj) else
      tdmStartOther = tdm$startOtherFunc;

    tdm$roi <- sC$alg.roi;
  
    maxEvaluations = sC$auto.loop.nevals/sC$seq.design.maxRepeats;
  
    tunerVal <- optim(par=param, fn=tdmStartOther, gr=NULL, method="L-BFGS-B", lower=roiLower, upper=roiUpper, control=list(maxit=maxEvaluations), opts=sC$opts)
    # optim calls tdmStartOther and tdmStartOther writes envT$res and envT$bst
  
    cat(">>>>>> RESULT:\n")
    print(tunerVal)
  
    tunerVal;
}


