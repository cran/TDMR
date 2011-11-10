######################################################################################
# unbiasedBestRun_O:
#
#'     Perform unbiased runs with best-solution parameters (optimization).
#'     Read the best solution of a optimization tuner run (either from envT$bst or from file), 
#'     perform a re-run  with these best parameters, to see whether the result quality is reproducible on 
#'     independent test data or on independently trained models.
#'
#'     If envT$bst or envT$res is NULL, try to read it from the file (the filename is
#'     given confFile or envT$spotConfig and we try to find it in dir envT$theTuner).
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
#'                      If NULL, create new one-row data frame finals with columns 
#'                      \preformatted{CONF  TUNER  NRUN  NEVAL Y  sdY}
#'   @param umode       ["DEF"] -- not used --
#'   @param withParams  [FALSE] if =TRUE, add columns with best parameters to data frame finals
#'                      (should be FALSE, if different runs have different parameters)
#'   @param tdm         a list with TDM settings from which we need here the elements
#'     \describe{
#'     \item{mainCommand}{ the command to be called for unbiased evaluations}
#'     \item{mainFile}{ change to the directory of mainFile before starting mainCommand}
#'     \item{nrun}{ how often to call the unbiased evaluation}
#'     }
#'   @return finals     a one-row data frame with final results
#'
#' @author Wolfgang Konen, FHK, Sep'2010 - May'2011
#' @export
#
######################################################################################
unbiasedBestRun_O <- function(confFile,envT,finals=NULL,umode="DEF",withParams=F,tdm=tdm){
    if (is.null(envT$spotConfig)) envT$spotConfig <- spotGetOptions(srcPath=tdm$theSpotPath,confFile);
    if (is.null(envT$theTuner)) envT$theTuner <- "lhd";
    if (is.null(envT$spotConfig$opts)) stop("List envT$spotConfig does not have the required variable 'opts'");
 	
    writeLines(paste("start unbiased run for",basename(tdm$mainCommand),"..."), con=stderr());
# --- this is now in tdmCompleteEval.r (before parallel execution branch) ---
#   pdFile = envT$spotConfig$io.apdFileName;
#  	source(pdFile,local=T)            # read problem design  (here: all elements of list opts)   

    opts <- envT$spotConfig$opts;

    bst <- tdmGetObj(envT$bst,envT$spotConfig$io.bstFileName,envT$theTuner,tdm);
    res <- tdmGetObj(envT$res,envT$spotConfig$io.resFileName,envT$theTuner,tdm);

    k <- nrow(bst);       # last line has the best solution
  	cat("Best solution:\n"); print(bst[k,]);

		conf <- bst$CONFIG[k]
		cat(sprintf("Best Config: %5d\n",conf))
		
    if (is.null(finals)) {
      # write a line with results to data frame :
      finals <- data.frame(list(CONF=sub(".conf","",confFile,fixed=TRUE),TUNER=envT$theTuner,NEXP=envT$nExp));
      if (withParams) {
        finals <- cbind(finals
                        ,tail(bst[,setdiff(names(bst[-1]),c("REPEATS","repeatsLastConfig","STEP","SEED","COUNT"))],1)
                        );
      } 
      finals <- cbind(finals
                     , NRUN=tdm$nrun
                     , NEVAL=nrow(res)
                     , Y=tail(bst[,1],1)
                     , sdY=sd(res[res$CONFIG==tail(bst$CONFIG,1),1])
                      );
    } # if(is.null(finals))
    
    # Solve the optimization or learning problem anew with the best tuning parameters.
    # (This makes of course only sense, if the optimization outcome is not deterministic, but 
    # contains some stochastic component)  
    if (tdm$nrun>0) {
      if (!is.null(tdm$fileProps)) {
    		# write fileProps (from bst):
    		paramNames = setdiff(names(bst),c("Y","COUNT","CONFIG"));
    		write.table(t(bst[k,paramNames]), file=paste(dirname(tdm$mainFile),tdm$fileProps,sep="/")
                    , quote=F, sep="=", dec=".", row.names=T, col.names=F);
  			write.table(t(opts), file=paste(dirname(tdm$mainFile),tdm$fileProps,sep="/")
                    , quote=F, sep="=", dec=".", row.names=T, col.names=F, append=T)
        tdm$mainCommand <- sub("fileProps",tdm$fileProps,tdm$mainCommand);
      }
      # solve problem anew, tdm$nrun times:                  
  		y_u = NULL;
  		for (n in 1:tdm$nrun) {
    		oldwd = getwd(); setwd(dirname(tdm$mainFile));              # save & change working dir
  			callString = tdm$mainCommand;
  			#print(callString);
 			
  			sres <- system(callString, intern= TRUE);
    		print(sres);
    		setwd(oldwd);                                     # restore working dir
    		y_u = c(y_u,as.numeric(sres[length(sres)]));      # the last element of sres is the value to be minimized
  		}
  
      finals <- cbind(finals, data.frame(Y_U=mean(y_u),sdY_U=sd(y_u)));
    } # if (tdm$nrun>0)
    
    finals;
}

#--- Example usage -------------------------------------------------------------
#
# finals <- unbiasedBestRun_O("rlearn_01.bst", tdm=tdm)
