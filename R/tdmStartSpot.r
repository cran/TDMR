######################################################################################
# tdmStartSpot
#
#' Function called by \code{\link{spot}} to evaluate a DM task during a \code{\link{SPOT}} tuning run.
#'
#' This function is called by \code{\link{spot}} for evaluations during a tuning run. It accumulates in 
#' \code{spotConfig$alg.currentResult} the RES data frame of all evaluations and in 
#' \code{spotConfig$alg.currentBest} the BST data frame of the so far best solution.
#'
#' \code{\link{spot}} is invoked from \code{\link{spotTuner}} or \code{\link{lhdTuner}}. 
#' The latter is simply a \link{SPOT}-run with all budget devoted to the initial design. 
#'
#' @param spotConfig    the list of configurations for \link{SPOT}. Besides the usual \link{SPOT} settings,
#'    this list has to contain an element \code{tdm} with the mandatory elements \itemize{
#'      \item tdm$mainFunc:     the function name of the DM task to execute (e.g. "main_sonar")
#'      \item tdm$mainCommand:  the R command to execute, usually \code{result <- <mainFunc>(opts,dset=dset)} where <mainFunc> is the string in tdm$mainFunc. 
#'          (It is expected that \code{mainCommand} returns \code{result} and that the element 
#'          \code{result$y} contains the quantity to be minimized by \link{SPOT}.)
#'      }
#'    Optionally, \code{tdm} may define  \itemize{
#'      \item tdm$mainFile:     e.g. "myDir/main_sonar.r", the R file of the DM task to source and \code{dirname(tdm$mainFile)} is the directory where tdm$mainFunc is executed.
#'      }
#' @return spotConfig
#'
#' @seealso \code{\link{spot}}, \code{\link{spotTuner}}, \code{\link{lhdTuner}}, \code{\link{tdmDispatchTuner}}
#' 
#' @author Wolfgang Konen
#' @export
######################################################################################
tdmStartSpot <- function(spotConfig) {
  	if (exists(".Random.seed")) SAVESEED<-.Random.seed	   #save the Random Number Generator RNG status
    if (!is.list(spotConfig)) stop("Error: spotConfig is not a list");
    if (is.null(spotConfig$opts)) stop("Error: spotConfig does not contain an element 'opts'");
    if (is.null(spotConfig$tdm)) stop("Error: spotConfig does not contain an element 'tdm'");
    if (is.null(spotConfig$tdm$fileMode)) spotConfig$tdm$fileMode <- TRUE;
        # This default setting is useful to allow a simpler TDM-Phase-2 call (with tdm$fileMode not def'd).
        # However, tdm has to specify the mandatory setting: either tdm$mainFunc or  tdm$mainFile + tdm$mainCommand
    tdm <- spotConfig$tdm;
    opts <- spotConfig$opts;
    dset <- switch(as.character(is.null(spotConfig$dataObj)),"TRUE"=NULL,"FALSE"=dsetTrnVa(spotConfig$dataObj));    
    # If dset is not NULL, this has an effect on tdm$mainCommand which contains "...,dset=dset".
    # If dset is NULL, the data reading is deferred to main_TASK.

  	writeLines("tdmStartSpot run...", con=stderr());   
# --- this is now for phase 3 in tdmCompleteEval.r (or for phase 2 in appropriate phase2-script) ---
#   pdFile <- spotConfig$io.apdFileName;
#  	print(pdFile);
#  	## read default problem design  (here: set default values for all elements of list opts)
#  	source(pdFile);                  # contains *no longer* the definition of tdm$mainFile & tdm$mainCommand
#
#   if (!is.null(tdm$mainFile")  source(tdm$mainFile);          

  	## read doe/dace etc settings:
  	if (spotConfig$spot.fileMode) {
      desFileName <-  spotConfig$io.desFileName;
    	writeLines(paste("Loading design file data from:", desFileName), con=stderr());
    	des <- read.table(desFileName
    			, sep=" "
    			, header = TRUE
    	);
  	} else {
      des <- spotConfig$alg.currentDesign;
  	}
  
    # round INT columns of data frame des and print its summary (see tdmMapDesign.r):	
  	des <- tdmMapDesInt(des,TRUE,spotConfig);     

  	config<-nrow(des);
  	print(config);
  	if (is.null(des$CONFIG))
  	   stop("Design file is missing the required column CONFIG!")
  	
  	#if (!all(des$REPEATS[1:config]>0))   {
  	#   warning("error des$REPEATS")
  	#   print(des$REPEATS)
    #}
  	for (k in 1:config){
  	
      des <- tdmMapCutoff(des,k,spotConfig);  # enforce CUTOFF parameter constraint if CUTOFF2[,3,4] appears in .des-file
      
  		for (r in 1:des$REPEATS[k]){
  	    opts$rep <- r;
      	opts <- tdmMapDesSpot$apply(des,opts,k,tdm);
   			
        if (!is.null(des$STEP))	theStep <- des$STEP[k];
  			opts$ALG.SEED <- des$SEED[k]+r;		# now used in tdmClassify, tdmClassifyLoop
  			
  			cat(sprintf("Config: %5d,   Repeat: %5d\n",des$CONFIG[k],r));
  			
  			oldwd = getwd(); 
        if (!is.null(tdm$mainFile)) setwd(dirname(tdm$mainFile));    # save & change working dir 		         			
    		result = NULL;     	
        eval(parse(text=tdm$mainCommand));                # execute the command given in string tdm$mainCommand
        if (is.null(result$y)) stop("tdm$mainCommand did not return a list 'result' containing an element 'y'");
  			setwd(oldwd);                                     # restore working dir 

        # append a line with results to result data frame spotConfig$alg.currentResult:
        res <- data.frame(list(Y=result$y
                              ,des[k,setdiff(names(des),c("CONFIG","REPEATS","repeatsLastConfig","STEP","SEED"))]
                              ));
        res <- cbind(res
          					,SEED=opts$ALG.SEED
         					  ,STEP=theStep
          					,CONFIG=des$CONFIG[k]                  
          					,REP=r
                    );
			  spotConfig$alg.currentResult=rbind(spotConfig$alg.currentResult,res);			
        # (alg.currentResult is initially set to NULL in spotTuner and lhdTuner, see tdmDispatchTuner.r)				

#fff <- function() {  browser() }; fff();  # this is just to halt here without the complete spotConfig printout (contains dataObj)

        if (tdm$fileMode) {                  
          resFileName <- spotConfig$io.resFileName;
    			colNames = ifelse(file.exists(resFileName),FALSE,TRUE);			
    			write.table(res
    					, file = resFileName
    					, row.names = FALSE
    					, col.names = colNames
    					, sep = " "              
    					, append = !colNames            
    					, quote = FALSE                    
    			);			
  			}
  		}	# for (r)			
  	}	# for (k)	
   	if (exists("SAVESEED")) assign(".Random.seed", SAVESEED, envir=globalenv()); 		#load the saved RNG status
  	return(spotConfig);            # new, *necessary* !!
}

