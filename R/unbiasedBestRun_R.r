######################################################################################
# unbiasedBestRun_R
#
#'     Perform unbiased runs with best-solution parameters (regression).
#'     Read the best solution of a parameter-tuning run (either from envT$bst or from file), 
#'     run with these best parameters the command \code{tdm$mainCommand} (usually a regression machine learning task), to see 
#'     whether the result quality is reproducible on independent test data or on independently trained models.
#'
#'     If envT$bst or envT$res is NULL, try to read it from the file (the filename is
#'     inferred from confFile or envT$spotConfig and we try to find it in dir envT$theTuner).
#' 
#'   @param confFile    the .conf filename, e.g. "appAcid_02.conf"
#'   @param envT        environment, from which we need the objects
#'     \describe{
#'     \item{\code{bst}}{ data frame containing best results (merged over repeats)}
#'     \item{\code{res}}{ data frame containing all results}
#'     \item{\code{theTuner}}{ ["spot"] string}
#'     \item{\code{opts}}{ list with all parameter settings for the DM task, i.e. as read in from  spotConfig$io.apdFileName}
#'     \item{\code{spotConfig}}{ [NULL] a list with SPOT settings. If NULL, try to read spotConfig from confFile.} 
#'     }
#'   @param finals      [NULL] a one-row data frame to which new columns with final results are added
#'   @param umode       [ "RSUB" | "CV" | "TST" ], how to divide in training and test data for the unbiased runs, default is "RSUB", 
#'                      see \code{tdmMapOpts} in tdmMapDesign.r for further details.
#'   @param withParams  [FALSE] if =TRUE, add columns with best parameters to data frame finals
#'                      (should be FALSE, if different runs have different parameters)
#'   @param tdm         a list with TDM settings from which we need here the elements
#'     \describe{
#'     \item{mainCommand}{ the command to be called for unbiased evaluations}
#'     \item{mainFile}{ change to the directory of mainFile before starting mainCommand}
#'     \item{nrun}{ how often to call the unbiased evaluation}
#'     }
#'   @return \code{finals}     a one-row data frame with final results
#'
#' @note Side Effects:
#'    The list \code{result} returned from \code{tdm$mainCommand} is written onto \code{envT$result}.
#'
#' @examples
#'    oldwd <- getwd();          
#'    setwd(paste(.find.package("TDMR"), "demo01cpu",sep="/"));
#'    envT <- new.env();
#'    tdm <- list(mainFile="main_cpu.r", mainCommand="result <- main_cpu(opts)");
#'    source(tdm$mainFile);
#'    finals <- unbiasedBestRun_R("cpu_01.conf",envT,tdm=tdm)
#'    print(finals);
#'    setwd(oldwd);
#'
#' @references   \code{\link{unbiasedBestRun_C}}
#' @author Wolfgang Konen, FHK, Sep'2010 - Oct'2011
#' @export
#
######################################################################################
unbiasedBestRun_R <- function(confFile,envT,finals=NULL,umode="RSUB",withParams=F,tdm=tdm){
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
      envT$spotConfig$opts <- opts;
    }

    writeLines(paste("start unbiased run (regression) for",basename(tdm$mainFile),"..."), con=stderr());
# --- this is now in tdmCompleteEval.r (before optional parallel execution) ---
#   pdFile = envT$spotConfig$io.apdFileName;
#  	source(pdFile,local=TRUE)            # read problem design  (here: all elements of list opts)   
#   source(tdm$mainFile)

    opts <- envT$spotConfig$opts;

    # set certain elements of opts which control selection of training & test data 
    # (depending on switch umode, see tdmMapDesign.r)
  	opts <- tdmMapOpts(umode,opts,tdm);

    bst <- tdmGetObj(envT$bst,envT$spotConfig$io.bstFileName,envT$theTuner,tdm);
    res <- tdmGetObj(envT$res,envT$spotConfig$io.resFileName,envT$theTuner,tdm);

    k <- nrow(bst);       # last line has the best solution
  	opts <- tdmMapDesApply(bst,opts,k,envT,tdm);
  	cat("Best solution:\n"); print(bst[k,]);

		conf <- bst$CONFIG[k]
		cat(sprintf("Best Config: %5d\n",conf))
		
		oldwd = getwd(); setwd(dirname(tdm$mainFile));        # save & change working dir
		result = NULL;             
    eval(parse(text=tdm$mainCommand));                    # execute the command given in text string tdm$mainCommand
    if (!is.list(result)) stop("tdm$mainCommand did not return a list 'result'");
		setwd(oldwd);                                         # restore working dir
    envT$result <- result;

    if (is.null(finals)) {
      # create a data frame with one line of results
      finals <- data.frame(list(CONF=sub(".conf","",confFile,fixed=TRUE),TUNER=envT$theTuner,NEXP=envT$nExp));
      if (withParams) {
        finals <- cbind(finals
                        ,bst[k,setdiff(names(bst[-1]),c("CONFIG","REPEATS","repeatsLastConfig","STEP","SEED","COUNT"))]
                        );
      } 
      finals <- cbind(finals
                     , NRUN=opts$NRUN
                     , NEVAL=nrow(res)
                     , RMAE.bst=-bst[k,1]
                     , RMAE.avg=-mean(res$Y)
                      );
      if (opts$method %in% c("RF","MC.RF")) {
        finals <-  cbind(finals
                       , RMAE.bst=-bst[k,1]
                       , RMAE.OOB=mean(result$R_train)
                       ,  sdR.OOB=sd(result$R_train)
                       );
      } else { 
        finals <-  cbind(finals
                       , RMAE.bst=-bst[k,1]
                       , RMAE.TRN=mean(result$R_train)
                       ,  sdR.TRN=sd(result$R_train)
                       );
      } 
    } # if(is.null(finals))
     
    add.finals <- data.frame(mean(result$R_test),sd(result$R_test));
    names(add.finals) <-  paste(c("RMAE.","sdR."),umode,sep="");
    finals <- cbind(finals,add.finals);

    finals;
}

