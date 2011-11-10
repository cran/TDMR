######################################################################################
#' Perform a \link{SPOT} tuning. Call \code{\link{spot}} and write the necessary information on environment \code{envT}
#'
#' @param confFile task configuration for tuning algorithm
#' @param spotStep needed for spot
#' @param theSpotPath needed for spot
#' @param tdm the TDMR object
#' @param envT the environment variable
#'
#' @return the result of SPOT tuning, i.e. the list \code{spotConfig}
#' @export
######################################################################################
spotTuner <- function(confFile,spotStep,theSpotPath,tdm,envT)
{
    sC <- envT$spotConfig;
    #sC$alg.func <- "tdmStartSpot";
    #sC$spot.fileMode = TRUE; 
    sC$tdm <- tdm;              # needed for tdm$mainFile, tdm$mainCommand, tdm$fileMode
    sC$alg.currentResult <- NULL;

    sC <- spot(confFile,spotStep,theSpotPath,sC);
    # spot calls tdmStartSpot. 
    # tdmStartSpot reads opts from sC$opts and writes sC$alg.currentResult

    envT$res <- sC$alg.currentResult;
    envT$bst <- sC$alg.currentBest;	
    sC;
}

######################################################################################
#' Perform LHD tuning. Perform a parameter tuning using a Latin hypercube design (LHD) 
#' for obtaining best design points.
#'
#' Configure SPOT so that all the budget is used for the initial design (usually LHD)
#' 
#' @param confFile task configuration for tuning algorithm
#' @param spotStep needed for spot
#' @param theSpotPath needed for spot
#' @param tdm the TDMR object
#' @param envT the environment variable
#'
#' @return the result of LHD sampling, i.e. the list \code{spotConfig}
#' @export
######################################################################################
lhdTuner <- function(confFile,spotStep,theSpotPath,tdm,envT)
{
    sC <- envT$spotConfig;
    sC$tdm <- tdm;              # needed for tdm$mainFile, tdm$mainCommand, tdm$fileMode
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

    sC <- spot(confFile,spotStep,theSpotPath,sC);
    # spot calls tdmStartSpot. 
    # tdmStartSpot reads opts from sC$opts and writes sC$alg.currentResult

    envT$res <- sC$alg.currentResult;
    envT$bst <- sC$alg.currentBest;	
    sC;
}


######################################################################################
#' Perform CMA-ES tuning. Perform a parameter tuning by CMA-ES, using the *R*-implementation 
#' (package \code{cmaes} from Olaf Mersmann).
#' 
#' @param confFile task configuration for tuning algorithm
#' @param tdm the TDMR object
#' @param envT the environment variable
#'
#' @return result of CMA-ES algorithm
#' @export
######################################################################################
cmaesTuner <- function(confFile,tdm,envT)
{
    require(cmaes)
    sC <- envT$spotConfig;
    roi <- sC$alg.roi;
    set.seed(sC$spot.seed);
    param <- runif(length(roi$low),roi$low,roi$high);#/2;    # start configuration

    if (tdm$fileMode) {
      tdm$resFile <-  sC$io.resFileName;   
      tdm$bstFile <-  sC$io.bstFileName;   
      if (file.exists(sC$io.resFileName)) file.remove(sC$io.resFileName);
      if (file.exists(sC$io.bstFileName)) file.remove(sC$io.bstFileName);
    }
    
    if(is.null(tdm$startOtherFunc)) tdmStartOther = makeTdmStartOther(tdm,envT) else
      tdmStartOther = tdm$startOtherFunc;


    #--- obsolete this is now all done in tdmCompleteEval and saved in sC$opts, before the parallel branches ---
    #pdFile <- sC$io.apdFileName;   
  	#writeLines(paste("Loading apd file data from:", pdFile), con=stderr());
  	#** read default problem design  (here: set default values for all elements of list opts)
  	#source(pdFile,local=TRUE)           # contains *no longer* the definition of tdm$mainFile & tdm$mainCommand
    #source(tdm$mainFile)
    
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
    #browser()
    tunerVal <- cma_es(param,tdmStartOther,sC$opts,lower=roi$low,upper=roi$high,control=control);
    # cma_es calls tdmStartOther and tdmStartOther appends to data frames envT$res and envT$bst.
    # If tdm$fileMode==TRUE, then tdmStartOther will write envT$res to envT$spotConfig$io.resFileName
    # and envT$bst to envT$spotConfig$io.bstFileName 
    
    cat(sprintf("Function calls to tdmStartOther: %d\n",tunerVal$count[1]));

    # OLD: 
    ##envT$bst <- read.table(sC$io.bstFileName, sep=" ", header = TRUE);	
 
    tunerVal;
}


######################################################################################
#' Perform CMA-ES tuning. Perform a parameter tuning by CMA-ES, using the *Java* 
#' implementation of Niko Hansen.
#' 
#' @param confFile task configuration for tuning algorithm
#' @param tdm the TDMR object
#' @param envT the environment variable
#'
#' @return result of CMA-ES algorithm
#' @export
######################################################################################
cma_jTuner <- function(confFile,tdm,envT)
{
    sC <- envT$spotConfig;
    roi <- sC$alg.roi;
    set.seed(sC$spot.seed);
    param <- runif(length(roi$low),roi$low,roi$high);#/2;    # start configuration

    if (tdm$fileMode) {
      tdm$resFile <-  sC$io.resFileName;   
      tdm$bstFile <-  sC$io.bstFileName;   
      if (file.exists(sC$io.resFileName)) file.remove(sC$io.resFileName);
      if (file.exists(sC$io.bstFileName)) file.remove(sC$io.bstFileName);
    }
    
    if(is.null(tdm$startOtherFunc)) tdmStartOther = makeTdmStartOther(tdm,envT) else
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

    #--- this is the normal version for javadir ---
    if (is.null(tdm$tdmPath)) {
        javadir=paste(.find.package("TDMR"),"javabin",sep="/");
    } else {
        javadir=paste(tdm$tdmPath,"inst","javabin",sep="/");
    }
    #--- next line is for the Eclipse developer version ---
    #javadir="C:/Dokumente und Einstellungen/wolfgang/Eigene Dateien/ProjectsWK/ReinforceLearn/cmaes_java/cma/bin"
    
		oldWD = getwd(); setwd(javadir);        # save & change working dir
    save.image(file="cma_j1.rda")                      # all function objects def'd in .GlobalEnv 
    save(list=ls(all=TRUE),file="cma_j2.rda")          # envT, tdm, tdmStartOther and other local variables
   
		write.table(t(c("dimension",length(roi$low))), file="CMAprops.txt"
                , quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE);
    s="initialX ="; for (i in 1:length(param)) s=paste(s,param[i]);                
		write.table(s, file="CMAprops.txt", quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE, append=TRUE)
    s="initialStandardDeviations ="; for (i in 1:length(roi$hig)) s=paste(s,(roi$hig[i]-roi$low[i])/2);
		write.table(s, file="CMAprops.txt", quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE, append=TRUE)
    s="lowerBounds ="; for (i in 1:length(roi$low)) s=paste(s,roi$low[i]);
		write.table(s, file="CMAprops.txt", quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE, append=TRUE)
    s="upperBounds ="; for (i in 1:length(roi$hig)) s=paste(s,roi$hig[i]);
		write.table(s, file="CMAprops.txt", quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE, append=TRUE)
		write.table(t(c("stopMaxFunEvals",min(fncall1,fncall2)/sC$seq.design.maxRepeats)), file="CMAprops.txt"
                , quote=FALSE, sep="=", dec=".", row.names=FALSE, col.names=FALSE, append=TRUE)
		callString = paste("java cma.examples.cmaJava CMAprops.txt");
		#print(callString);
	
		sres <- system(callString, intern= TRUE);
		if (length(grep("cma_j.r returned with error status",sres))>0) {
		  print(sprintf("%s",sres));
		  stop("Error in cma_jTuner: cma_j.r returned with error status >> check cma_j.err.");
    }
    # cmaJava calls repeatedly "R cma_j.r" which reloads cma_j1.rda, cma_j2.rda and then executes tdmStartOther.
    # (Be aware that on Windows the path to R.exe must be in the system variable Path)
    # tdmStartOther appends to data frames envT$res and envT$bst.
    # If tdm$fileMode==TRUE, then tdmStartOther will write envT$res to envT$spotConfig$io.resFileName
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
		
    tunerVal = list();
    tunerVal$sres <- sres; 
    tunerVal$count=nrow(envT$res);
    
    cat(sprintf("Function calls to tdmStartOther: %d\n",tunerVal$count[1]));

    tunerVal;
}


######################################################################################
#' Perform Powell's tuning. Perform a parameter tuning by Powell's UObyQA algorithm 
#' (unconstrained optimization by quadratic approximation), see R-package \code{powell}
#' 
#' @param confFile task configuration for tuning algorithm
#' @param tdm the TDMR object
#' @param envT the environment variable
#'
#' @return result of Powell's algorithm
#' @export
######################################################################################
powellTuner <- function(confFile,tdm,envT){
    require(powell)
    #require(SPOT)
  
    #function to fix constraint violating steps:
    tdm$constraintFnc <- function(x,tdm) {
      for(i in 1:length(x)){
        if(x[i] < envT$spotConfig$alg.roi[i,1]) {x[i] <- envT$spotConfig$alg.roi[i,1]}
        if(x[i] > envT$spotConfig$alg.roi[i,2]) {x[i] <- envT$spotConfig$alg.roi[i,2]}
      }
      x;
    }
     
    sC <- envT$spotConfig;
    roi <- sC$alg.roi;
  
    if (tdm$fileMode) {
      tdm$resFile <-  sC$io.resFileName;   
      tdm$bstFile <-  sC$io.bstFileName;   
      if (file.exists(sC$io.resFileName)) file.remove(sC$io.resFileName);
      if (file.exists(sC$io.bstFileName)) file.remove(sC$io.bstFileName);
    }
  
    if(is.null(tdm$startOtherFunc)) tdmStartOther = makeTdmStartOther(tdm,envT) else
      tdmStartOther = tdm$startOtherFunc;

    #dset <- read.table("appAcid_32.exp", sep=" ", header=TRUE)
    #param <- as.numeric(dset[1,3:14])
  
    #if(param==NULL){
    #  param <- runif(length(roi$low), min=roi$low, max=roi$high) # create start vector generating uniformly randomized vector
    #}
  
    #cat(">>>>> ")
    #cat(param)
    #cat("\n")
    param <- (roi$low+roi$high)/2;    # start configuration
  
    #--- obsolete this is now all done in tdmCompleteEval and saved in sC$opts, before the parallel branches ---
    #pdFile <- sC$io.apdFileName;   
  	#writeLines(paste("Loading apd file data from:", pdFile), con=stderr());
  	#** read default problem design  (here: set default values for all elements of list opts)
  	#source(pdFile,local=TRUE)           # contains *no longer* the definition of tdm$mainFile & tdm$mainCommand
    #source(tdm$mainFile)
    
    maxEvaluations = sC$auto.loop.nevals/sC$seq.design.maxRepeats;
  
    tunerVal <- powell(param, fn=tdmStartOther, control=list(maxit=maxEvaluations), check.hessian=FALSE, opts=sC$opts);
    # powell calls tdmStartOther and tdmStartOther writes envT$res and envT$bst

    # OLD: 
    #tunerVal <- powell(param, fn=tdmStartPowell, control=list(maxit=maxEvaluations), check.hessian=FALSE, opts=sC$opts, tdm=tdm);
    
    # OLD: 
    ##envT$bst <- read.table(sC$io.bstFileName, sep=" ", header = TRUE);	
    
    tunerVal;
  
}


######################################################################################
#' Perform BFGS tuning. Perform a parameter tuning by Broyden, Fletcher, Goldfarb and Shanno (BFGS) method.
#' The L-BFGS-B version allowing box constraints is used here.
#' 
#' @param confFile task configuration for tuning algorithm

#' @param tdm the TDMR object
#' @param envT the environment variable
#'
#' @return result of BFGS algorithm
#' @author Wolfgang Konen, Patrick Koch \email{wolfgang.konen@@fh-koeln.de}
#' @export
######################################################################################
bfgsTuner <- function(confFile,tdm,envT){
    #require("optimx")
    #browser()
    sC <- envT$spotConfig;
    roi <- sC$alg.roi;                 
    param <- runif(length(roi$low), min=roi$low, max=roi$high) # create start vector generating uniformly randomized vector
    #dset <- read.table("appAcid_32.bst", sep=" ", header=TRUE) # for finetuning run on previous results
    #param <- as.numeric(dset[14,2:13])
  
    #cat("PARAM INITIAL:")
    #print(param)
    #param <- (roi$low+roi$high)/2;    # start configuration
  
    #--- obsolete this is now all done in tdmCompleteEval and saved in sC$opts, before the parallel branches ---
    #pdFile <- sC$io.apdFileName;   
  	#writeLines(paste("Loading apd file data from:", pdFile), con=stderr());
  	#** read default problem design  (here: set default values for all elements of list opts)
  	#source(pdFile,local=TRUE)           # contains *no longer* the definition of tdm$mainFile & tdm$mainCommand
    #source(tdm$mainFile)
     
    if (tdm$fileMode) {
      tdm$resFile <-  sC$io.resFileName;   
      tdm$bstFile <-  sC$io.bstFileName;   
      if (file.exists(sC$io.resFileName)) file.remove(sC$io.resFileName);
      if (file.exists(sC$io.bstFileName)) file.remove(sC$io.bstFileName);
    }
  
    if(is.null(tdm$startOtherFunc)) tdmStartOther = makeTdmStartOther(tdm,envT) else
      tdmStartOther = tdm$startOtherFunc;

    tdm$roi <- roi;
  
    maxEvaluations = sC$auto.loop.nevals/sC$seq.design.maxRepeats;
  
    tunerVal <- optim(par=param, fn=tdmStartOther, gr=NULL, method="L-BFGS-B", lower=roi$low, upper=roi$high, control=list(maxit=maxEvaluations), opts=sC$opts)
    # optim calls tdmStartOther and tdmStartOther writes envT$res and envT$bst
  
    # OLD: 
    #tunerVal <- optim(par=param, fn=tdmStartBFGS, gr=NULL, method="L-BFGS-B", lower=roi$low, upper=roi$high, control=list(maxit=maxEvaluations), opts=sC$opts, tdm=tdm)
    
    cat(">>>>>> RESULT:\n")
    print(tunerVal)
  
    # OLD: 
    #envT$bst <- read.table(sC$io.bstFileName, sep=" ", header = TRUE);	
  
    tunerVal;

}


###########################################################################################
#' tdmDispatchTuner:
#'     helper function for tdmCompleteEval:
#'     select and start the tuner specified by tuneMethod
#' @param tuneMethod the tuning algorithm given as a char. Possible options are ("spot", "lhd", "cmaes", "bfgs", "powell").
#' @param confFile the configuration file.
#' @param spotStep step for spot. At the moment "rep" and "auto" are supported by TDMR.
#' @param tdm The TDMR object
#' @param envT The environment variable.
#'
#' @return the result of the tuning algorithm.
#' @export
###########################################################################################
tdmDispatchTuner <- function(tuneMethod,confFile,spotStep,tdm,envT)
{
    theSpotPath=tdm$theSpotPath
    
    if (spotStep=="auto") {
      if (is.null(tdm$mainFile)) stop("Element tdm$mainFile is missing, but this is required for spotStep='auto'");
      if (is.null(tdm$mainCommand)) stop("Element tdm$mainCommand is missing, but this is required for spotStep='auto'");
    }
    
    tunerVal = switch(spotStep
      ,"rep" = spot(confFile,spotStep,theSpotPath)
      ,"auto" = switch(tuneMethod
                ,"spot" = spotTuner(confFile,spotStep,theSpotPath,tdm,envT)
                ,"lhd" = lhdTuner(confFile,spotStep,theSpotPath,tdm,envT) 
                ,"cmaes" = cmaesTuner(confFile,tdm,envT) 
                ,"cma_j" = cma_jTuner(confFile,tdm,envT) 
                ,"bfgs" = bfgsTuner(confFile,tdm,envT) 
                ,"powell" = powellTuner(confFile,tdm,envT) 
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
    
    tunerVal;
}
