######################################################################################
# unbiasedRun
#
#' Perform unbiased runs with best-solution parameters.
#'
#' Read the best solution of a parameter-tuning run from \code{envT$bst}, 
#' execute with these best parameters the function \code{tdm$mainFunc} (usually a classification or 
#' regression machine learning task), to see 
#' whether the result quality is reproducible on independent test data or on independently trained models.
#'
#' @param confFile    the configuration name, e.g. \code{"appAcid_02.conf"}
#' @param envT        environment, from which we need the objects
#'   \describe{
#'     \item{\code{bst}}{ data frame containing best results (merged over repeats)}
#'     \item{\code{res}}{ data frame containing all results}
#'     \item{\code{theTuner}}{ ["spot"] string}
#'     \item{\code{spotConfig}}{ [NULL] a list with SPOT settings. If NULL, try to read spotConfig from confFile.} 
#'     \item{\code{finals}}{ [NULL] a one-row data frame to which new columns with final results are added}
#'   }
#' @param dataObj     [NULL] contains the pre-fetched data with training-set and test-set part. 
#'                    If NULL, set it to \code{tdmReadAndSplit(opts,tdm)}. \cr
#'                    It is now \strong{deprecated} to have dataObj==NULL.  
# @param finals      [NULL] a one-row data frame to which new columns with final results are added ***now obsolete, we use envT$finals***
#' @param umode       --- deprecated as argument to unbiasedRun --- , use the division provided 
#'                    in \code{dataObj = tdmReadAndSplit(opts,tdm)} which makes use of \code{tdm$umode}.\cr
#'                    For downward compatibility only (if \code{dataObj==NULL} :\cr
#'                    [ "RSUB" (default) | "CV" | "TST" | "SP_T" ], how to divide in training and test data for the unbiased runs:
#'   \describe{
#'     \item{\code{"RSUB"}}{ random subsampling into (1-tdm$TST.testFrac)\% training and tdm$TST.testFrac\% test data}
#'     \item{\code{"CV"}}{ cross validation (CV) with tdm$nrun folds}
#'     \item{\code{"TST"}}{ all data in opts$filename (or dsetTrnVa(dataObj)) are used for training, 
#'                  all data in opts$filetest (or dsetTest(dataObj) are used for testing}
#'     \item{\code{"SP_T"}}{ 'split_test': prior to tuning, the data set was split by random subsampling into tdm$TST.testFrac\% test and
#'                  (1-tdm$TST.testFrac)\% training-vali data, tagged via column "tdmSplit". Tuning was done on training-vali data.
#'                  Now we use column "tdmSplit" to select the test data for unbiased evaluation. Training during unbiased evaluation
#'                  is done on a fraction tdm$TST.trnFrac of the training-vali data}
#'   }
#' @param withParams  [FALSE] if =TRUE, add columns with best parameters to data frame \code{finals}
#'                    (should be FALSE, if different runs have different parameters)
#' @param tdm         a list with TDM settings from which we use here the elements
#'   \describe{
#'     \item{mainFunc}{ the function to be called for unbiased evaluations}
#'     \item{mainFile}{ change to the directory of mainFile before starting mainFunc}
#'     \item{nrun}{ [5] how often to call the unbiased evaluation}
#'     \item{nfold}{ [10] how many folds in CV (only relevant for umode="CV") }
#'     \item{TST.testFrac}{ [0.2] test set fraction (only relevant for umode="RSUB" or ="SP_T") }
#'   }
#'   The defaults in '[...]' are set by  \code{\link{tdmDefaultsFill}}, if they are not defined on input.
#'   
#' @return \code{envT} the augmdented environment envT, with the following items updated
#'    \item{finals}{the final results}
#'    \item{tdm}{the updated list with TDM settings}
#'    \item{results}{last results (from last unbiased training)}
#'    
#'
#' @note Side Effects:
#'    The list \code{result}, an object of class \code{\link{TDMclassifier}} or \code{\link{TDMregressor}} as returned 
#'    from \code{tdm$mainFunc} is written onto \code{envT$result}. \cr
#'    If  \code{envT$spotConfig} is NULL, it is constructed from confFile. \cr
#'    \code{spotConfig$opts} (list with all parameter settings for the DM task) has to be non-NULL. 
#'
#' @examples
#'    ## Load the best results obtained in a prior tuning for the configuration "sonar_04.conf"
#'    ## with tuning method "spot". The result envT from a prior run of tdmBigLoop with this .conf
#'    ## is read from demo02sonar/demoSonar.RData.
#'    ## Run task main_sonar again with these best parameters, using the default settings from 
#'    ## tdmDefaultsFill: umode="RSUB", tdm$nrun=5  and tdm$TST.testFrac=0.2.
#'    path = paste(find.package("TDMR"), "demo02sonar",sep="/")
#'    envT = tdmEnvTLoad("demoSonar.RData",path);    # loads envT
#'    source(paste(path,"main_sonar.r",sep="/"));
#'    envT$tdm$optsVerbosity=1;
#'    envT$sCList[[1]]$opts$path=path;       # overwrite a possibly older stored path
#'    envT$spotConfig <- envT$sCList[[1]];
#'    dataObj <- tdmReadTaskData(envT,envT$tdm);
#'    envT <- unbiasedRun("sonar_04.conf",envT,dataObj,tdm=envT$tdm);
#'    print(envT$finals);
#'
#' @seealso   \code{\link{tdmBigLoop}}, \code{\link{TDMclassifier}}, \code{\link{TDMregressor}} 
#' @export
#' @author Wolfgang Konen, THK, 2013 - 2018
#' 
######################################################################################
unbiasedRun <- function(confFile,envT,dataObj=NULL,umode="RSUB",
                        withParams=FALSE,tdm=NULL){
    tdm <- tdmDefaultsFill(tdm);
    finals <- envT$finals;
    if (is.null(envT$spotConfig)) stop("envT does not contain the required object spotConfig");
    if (is.null(envT$theTuner)) envT$theTuner <- "spot";
    if (is.null(envT$nExp)) envT$nExp <- 1;
    if (is.null(tdm$map)) tdm <- tdmMapDesLoad(tdm); 
    #if (is.null(envT$map)) tdmMapDesLoad(envT,tdm); 
    
    if (is.null(envT$spotConfig$opts)) {
      if (is.null(tdmEnvTGetOpts(envT,1))) {
        stop("List envT$spotConfig does not have the required variable 'opts'");
      } else {
        opts <- tdmEnvTGetOpts(envT,1);
      }
    } else {
      opts <- envT$spotConfig$opts;
    }
  	opts$ALG.SEED <- envT$spotConfig$alg.seed;

    envT$tdm <- tdm;    # just as information to the caller of this function

    writeLines(sprintf("start unbiased run with function \"%s\" ...",tdm$mainFunc), con=stderr());

  	if (opts$READ.INI) {
  	  if (is.null(dataObj)) dataObj <- tdmReadAndSplit(opts,tdm);
      tdm$umode="RSUB"  # this is the only allowed case for umode, if dataObj is given.
                        # Then tdmMapOpts will set opts$TST.kind="rand"
      dset <- dsetTrnVa(dataObj,envT$nExp);
      tset <- dsetTest(dataObj,envT$nExp);       # returns NULL in case of (tdm$umode %in% c("RSUB","CV")
    } else {
      # this branch is deprecated 
      if (is.null(dataObj))
        cat("NOTE: setting opts$READ.INI==FALSE together with argument dataObj==NULL in unbiasedRun.r is deprecated. Consider opts$READ.INI=TRUE.");  
      dset <- NULL;
      tset <- NULL;
    }

  	tdmOpts <- tdmMapOpts(umode,opts,tdm);           # sets opts$NRUN = tdm$nrun
  	
    # tdmGetObj is deprecated, only for downward compatibility. For actual runs, a simple
       bst <- envT$bst
       res <- envT$res
    # is sufficient.
    # The old (deprecated) version was:  
#'     If envT$bst or envT$res is NULL, try to read it from the file (the filename is
#'     inferred envT$spotConfig. If this is NULL, it is constructed from confFile). 
#'     We try to find the files for envT$bst or envT$res in dir envT$theTuner).
#    bst <- tdmGetObj(envT$bst,envT$spotConfig$io.bstFileName,envT$theTuner,tdm);
#    res <- tdmGetObj(envT$res,envT$spotConfig$io.resFileName,envT$theTuner,tdm);

    k <- nrow(bst);       # last line has the best solution
    #bst <- tdmMapCutoff(bst,k,envT$spotConfig);  # enforce CUTOFF parameter constraint if CUTOFF2[,3,4] appears in .des-file
    if (!is.data.frame(bst)) {
      # this is for SPOT 2.0, where envT$bst = res$xbest (a matrix)
      bst <- as.data.frame(bst)
      pNames=row.names(envT$spotConfig$alg.roi);
      names(bst) <- pNames
      # (we need bst to be a data frame in tdmMapDesApply and in finals-creation below)
    }
  	tdmOpts <- tdmMapDesApply(bst,tdmOpts,k,envT$spotConfig,tdm);
  	if (is.null(tdmOpts$fileMode)) tdmOpts$fileMode=TRUE;    # might be necessary for older opts from file

  	cat("Best solution:\n"); print(bst[k,]);
  	#
  	# now tdmOpts has the best solution obtained in a prior tuning, and it has 
  	# training and test set configured according to the current setting of umode

  	if (is.data.frame(bst)) {
  	  conf <- as.numeric(bst$CONFIG[k])
  	  cat(sprintf("Best Config: %5d\n\n",conf))
  	}
    #
    # run new model trainings for the configuration stored in tdmOpts (tdm$nrun trainings & evaluations):
    #
#  	result <- main_sonar(tdmOpts,dset=dset,tset=tset)
		result <- eval(call(tdm$mainFunc,tdmOpts,dset=dset,tset=tset))            # tdm$mainFunc has to return result$y
		if (!is.list(result)) stop("tdm$mainFunc did not return a list 'result'");
		envT$result <- result;

    # experimental (problematic: can result in a 39 MB file)
    #saveEnvT(envT,tdm$runList,saveModel=tdm$U.saveModel);

    if (is.null(finals)) {
      # add results from tuning:
      #
      # create a data frame with one line of results:
      finals <- data.frame(list(CONF=sub(".conf","",confFile,fixed=TRUE),TUNER=envT$theTuner,NEXP=envT$nExp));
      namFinals <- names(finals);
      if (withParams) {
        pNames=row.names(envT$spotConfig$alg.roi);
        finals <- cbind(finals,bst[k,pNames]);    # bug fix 05/12: this way it works for length(pNames)==1 and for >1    
        names(finals) <- c(namFinals,pNames);     #
      } 
      finals <- cbind(finals, NEVAL=nrow(res));
      
      sgn <- ifelse(class(envT$result)[1]=="TDMregressor",+1,-1);   # minus sign only for classification
      add.finals        <- data.frame(sgn*as.numeric(bst[k,"Y"]),sgn*mean(res$Y));
      names(add.finals) <- paste(tdmOpts$rgain.string,c(".bst",".avg"),sep="");
      finals <- cbind(finals,add.finals);
      finals <- cbind(finals,Time.TRN=envT$time.TRN);  
      #
    } # if(is.null(finals))
      
		# add results frum unbiased runs:
		#
		finals <- cbind(finals, NRUN=tdmOpts$NRUN);                      
		# add results on the training set from unbiased runs:                      
		add.finals <-  data.frame( mean(result$R_train)
		                           , sd(result$R_train)
		);
		names(add.finals) <- paste(c(tdmOpts$rgain.string,"sdR"),".TRN",sep="");
		### older version, too complicated
		#suf = ifelse(tdmOpts$MOD.method %in% c("RF","MC.RF"),".OOB",".TRN");
		#names(add.finals) <- paste(c(tdmOpts$rgain.string,"sdR"),suf,sep="");
		finals <- cbind(finals,add.finals);
		#
		# add results on test set from unbiased runs:
    add.finals <- data.frame(mean(result$R_vali),sd(result$R_vali));
    names(add.finals) <-  paste(c(tdmOpts$rgain.string,"sdR"),umode,sep=".");
		finals <- cbind(finals,add.finals);
    envT$finals <- finals;
		
    envT;
}

######################################################################################
# zzz
#
#' just to make example check happy
#'
#' If we do not have this little dummy function with the plot command in the example, 
#' then 'R CMD check' in buildTDMR.r will fail when the examples are checked. 
#' The reason: The file TDMR.Rcheck/TDMR-Ex.R contains a footer which tries to 
#' close a graphics device with grDevices::dev.off(). But if there is no graphics device
#' from the previous example, dev.off() tries to operate on the null device and this
#' produces an error. 
#'
#' @return \code{dummy}     zero
#'
#' @examples
#'    plot(1:10);
#'    
#'
#' @keywords internal
#' @author Wolfgang Konen, FHK, 2010 - 2013
#' 
zzz <- function() {
  return(0)
}