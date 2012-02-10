######################################################################################
# unbiasedRun
#
#'     Perform unbiased runs with best-solution parameters.
#'     Read the best solution of a parameter-tuning run (either from envT$bst or from file), 
#'     run with these best parameters the command \code{tdm$mainCommand} (usually a classification or 
#'     regression machine learning task), to see 
#'     whether the result quality is reproducible on independent test data or on independently trained models.
#'
#'     If envT$bst or envT$res is NULL, try to read it from the file (the filename is
#'     inferred envT$spotConfig. If this is NULL, it is constructed from confFile). 
#'     We try to find the files for envT$bst or envT$res in dir envT$theTuner).
#'
#'   @param confFile    the .conf filename, e.g. "appAcid_02.conf"
#'   @param envT        environment, from which we need the objects
#'     \describe{
#'     \item{\code{bst}}{ data frame containing best results (merged over repeats)}
#'     \item{\code{res}}{ data frame containing all results}
#'     \item{\code{theTuner}}{ ["spot"] string}
#'     \item{\code{spotConfig}}{ [NULL] a list with SPOT settings. If NULL, try to read spotConfig from confFile.} 
#'     }
#'   @param dataObj     [NULL] contains the pre-fetched data from which we use here the test-set part
#'   @param finals      [NULL] a one-row data frame to which new columns with final results are added
#'   @param umode       [ "RSUB" (default) | "CV" | "TST" | "SP_T" ], how to divide in training and test data for the unbiased runs:
#'     \describe{
#'     \item{\code{"RSUB"}}{ random subsampling into 80\% training and 20\% test data}
#'     \item{\code{"CV"}}{ cross validation (CV) with tdm$nrun folds}
#'     \item{\code{"TST"}}{ all data in opts$filename are used for training, all data in opts$filetest for testing}
#'     \item{\code{"SP_T"}}{ 'split_test': the data set is split by random subsampling into test and training data}
#'     }
#'   @param withParams  [FALSE] if =TRUE, add columns with best parameters to data frame finals
#'                      (should be FALSE, if different runs have different parameters)
#'   @param tdm         a list with TDM settings from which we use here the elements
#'     \describe{
#'     \item{mainCommand}{ the command to be called for unbiased evaluations}
#'     \item{mainFile}{ change to the directory of mainFile before starting mainCommand}
#'     \item{nrun}{ [5] how often to call the unbiased evaluation}
#'     \item{nfold}{ [10] how many folds in CV (only relevant for umode="CV") }
#'     \item{tstFrac}{ [0.2] test set fraction (only relevant for umode="RSUB") }
#'     }
#'     The defaults in '[...]' are set by  \code{\link{tdmDefaultsFill}}, if they are not defined on input.
#'   @return \code{finals}     a one-row data frame with final results
#'
#' @note Side Effects:
#'    The list \code{result} returned from \code{tdm$mainCommand} is written onto \code{envT$result}. \cr
#'    If  \code{envT$spotConfig} is NULL, it is constructed from confFile. \cr
#'    If \code{spotConfig$opts} (list with all parameter settings for the DM task) is NULL, we try to read it from  
#'    \code{spotConfig$io.apdFileName}. This will issue a warning '... might not work in parallel mode', but is perfectly fine for non-parallel mode. 
#'
#' @examples
#'    # Load the best results obtained in a prior tuning for the configuration "sonar_04.conf" with tuning method "spot".
#'    # Run task main_sonar again with these best parameters, using umode="RSUB" and from tdmDefaultsFill 
#'    # the default settings tdm$nrun=5, tdm$tstFrac=0.2.
#'    oldwd <- getwd();          
#'    # The best results are read from demo02sonar/spot/sonar_04.bst relative to the TDMR package directory.
#'    setwd(paste(.find.package("TDMR"), "demo02sonar",sep="/"));
#'    envT <- new.env();
#'    tdm <- list(mainFile="main_sonar.r", mainFunction="main_sonar", tuneMethod="spot");
#'    if (!is.null(tdm$mainFile)) source(tdm$mainFile);
#'    finals <- unbiasedRun("sonar_04.conf",envT,tdm=tdm)
#'    print(finals);
#'    setwd(oldwd);
#'
#' @seealso   \code{\link{tdmCompleteEval}}
#' @author Wolfgang Konen, FHK, Sep'2010 - Dec'2011
#' @export
#
######################################################################################
unbiasedRun <- function(confFile,envT,dataObj=NULL,finals=NULL,umode="RSUB",withParams=F,tdm=NULL){
    tdm <- tdmDefaultsFill(tdm);
    if (is.null(envT$spotConfig)) envT$spotConfig <- spotGetOptions(srcPath=tdm$theSpotPath,confFile);
    if (is.null(envT$theTuner)) envT$theTuner <- "spot";
    if (is.null(envT$nExp)) envT$nExp <- 1;
    if (is.null(envT$map)) tdmMapDesLoad(envT,tdm); 
    if (is.null(envT$spotConfig$opts)) {
      pdFile = envT$spotConfig$io.apdFileName;
      warning(paste("List envT$spotConfig does not have the required variable 'opts'."
                   ,"We try to construct it from",pdFile,"(might not work in parallel mode!)")); 
      source(pdFile,local=TRUE);
      opts=tdmOptsDefaultsFill(opts);
      envT$spotConfig$opts <- opts;
    }
    envT$tdm <- tdm;    # just as information to the caller of this function

    writeLines(paste("start unbiased run for command \"", tdm$mainCommand,"\" ...",sep=""), con=stderr());
# --- this is now in tdmCompleteEval.r (before optional parallel execution) ---
#   pdFile = envT$spotConfig$io.apdFileName;
#  	source(pdFile,local=TRUE)         # read problem design  (here: all elements of list opts)   
#   if (!is.null(tdm$mainFile)) source(tdm$mainFile)

    opts <- envT$spotConfig$opts;
  	opts$ALG.SEED <- envT$spotConfig$alg.seed;
	
  	if (opts$READ.INI) {
      tdm$tstCol=dataObj$TST.COL;  # needed for tdmMapOpts
      dset <- dataObj$dset;    
    } else {
      dset <- NULL;
    }
    # set certain elements of opts which control selection of training & test data 
    # (depending on switch tdm$umode, see tdmMapDesign.r)
  	opts <- tdmMapOpts(umode,opts,tdm);           # sets opts$NRUN = tdm$nrun
  	
    
    bst <- tdmGetObj(envT$bst,envT$spotConfig$io.bstFileName,envT$theTuner,tdm);
    res <- tdmGetObj(envT$res,envT$spotConfig$io.resFileName,envT$theTuner,tdm);

    k <- nrow(bst);       # last line has the best solution
    bst <- tdmMapCutoff(bst,k,envT$spotConfig);  # enforce CUTOFF parameter constraint if CUTOFF2[,3,4] appears in .des-file
  	opts <- tdmMapDesApply(bst,opts,k,envT,tdm);
  	cat("Best solution:\n"); print(bst[k,]);
  	#
  	# now opts has the best solution obtained in a prior tuning, and it has 
  	# training and test set configured according to the current setting of umode

		conf <- bst$CONFIG[k]
		cat(sprintf("Best Config: %5d\n\n",conf))

    #
    # run new model trainings for the configuration stored in opts (tdm$nrun trainings & evaluations):
    #
		oldwd = getwd();                                               #
    if (!is.null(tdm$mainFile)) setwd(dirname(tdm$mainFile));      # save & change working dir
		result = NULL;         
    eval(parse(text=tdm$mainCommand));                  # execute the command given in text string tdm$mainCommand
    if (!exists("result")) stop("tdm$mainCommand did not return a list 'result'");
    if (!is.list(result)) stop("tdm$mainCommand did not return a list 'result'");
		setwd(oldwd);                                                  # restore working dir
    envT$result <- result;

    if (is.null(finals)) {
      # create a data frame with one line of results:
      finals <- data.frame(list(CONF=sub(".conf","",confFile,fixed=TRUE),TUNER=envT$theTuner,NEXP=envT$nExp));
      if (withParams) {
        finals <- cbind(finals
                        ,bst[k,setdiff(names(bst[-1]),c("CONFIG","REPEATS","repeatsLastConfig","STEP","SEED","COUNT"))]
                        );
      } 
      finals <- cbind(finals
                     , NRUN=opts$NRUN
                     , NEVAL=nrow(res)
                      );
      add.finals        <- data.frame(-bst[k,1],-mean(res$Y));
      names(add.finals) <- paste(opts$rgain.string,c(".bst",".avg"),sep="");
      finals <- cbind(finals,add.finals);
      #
      # add once the results on the training set from unbiased runs:                      
      suf = ifelse(opts$MOD.method %in% c("RF","MC.RF"),".TRN",".OOB");
      add.finals <-  data.frame( mean(result$R_train)
                               , sd(result$R_train)
                               );
      names(add.finals) <- paste(c(opts$rgain.string,"sdR"),suf,sep="");
      finals <- cbind(finals,add.finals);
    } # if(is.null(finals))
      
    # add results on test set from unbiased runs (two columns for each value of umode):
    add.finals <- data.frame(mean(result$R_test),sd(result$R_test));
    names(add.finals) <-  paste(c(opts$rgain.string,"sdR"),umode,sep=".");
    finals <- cbind(finals,add.finals);

    finals;
}

