######################################################################################
# tdmStartSpot
#
#' Start a tuning evaluation of a DM task for SPOT and LHD tuner
#'
#' This function is called by \code{spot} for tuning evaluations. It accumulates in 
#' \code{spotConfig$alg.currentResult} the RES data frame of all evaluations and in 
#' \code{spotConfig$alg.currentBest} the BST data frame of the so far best solution.
#'
#' @param spotConfig    the list of configurations for SPOT. Besides the usual SPOT settings,
#'    this list has to contain an element \code{tdm} with the mandatory elements \itemize{
#'      \item tdm$mainFile:     the R file of the DM task to source
#'      \item tdm$mainCommand:  the R command to execute, usually \code{result <- main_TASK(opts)}. 
#'          (It is expected that \code{mainCommand} returns \code{result} and that the element 
#'          \code{result$y} contains the quantity to be minimized by SPOT.)
#'      }
#' @return spotConfig
#'
#' @seealso \code{\link{spotTuner}}, \code{\link{lhdTuner}}, \code{\link{tdmDispatchTuner}}
#' 
#' @author Wolfgang Konen
#' @export
######################################################################################
tdmStartSpot <- function(spotConfig) {
    if (!is.list(spotConfig)) stop("Error: spotConfig is not a list");
    if (is.null(spotConfig$opts)) stop("Error: spotConfig does not contain an element 'opts'");
    if (is.null(spotConfig$tdm)) stop("Error: spotConfig does not contain an element 'tdm'");
    if (is.null(spotConfig$tdm$fileMode)) spotConfig$tdm$fileMode <- TRUE;
        # This default setting is useful to allow a simpler TDM-Phase-2 call (with tdm$fileMode not def'd).
        # However, tdm has to specify the mandatory settings tdm$mainFile, tdm$mainCommand
    tdm <- spotConfig$tdm;
    opts <- spotConfig$opts;
    
  	writeLines("tdmStartSpot run...", con=stderr());   
# --- this is now for phase 3 in tdmCompleteEval.r (or for phase 2 in appropriate phase2-script) ---
#   pdFile <- spotConfig$io.apdFileName;
#  	print(pdFile);
#  	## read default problem design  (here: set default values for all elements of list opts)
#  	source(pdFile);                  # contains *no longer* the definition of tdm$mainFile & tdm$mainCommand
#
#    source(tdm$mainFile);          

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
  	
      	opts <- tdmMapDesSpot$apply(des,opts,k,tdm);
   			
        if (!is.null(des$STEP))	theStep <- des$STEP[k];
  			seed <- des$SEED[k]+r;		# probably not used	
  			
  			cat(sprintf("Config: %5d,   Repeat: %5d\n",des$CONFIG[k],r));
  			
  			oldwd = getwd(); setwd(dirname(tdm$mainFile));    # save & change working dir 		         			
    		result = NULL; 
        eval(parse(text=tdm$mainCommand));                # execute the command given in string tdm$mainCommand
        if (is.null(result$y)) stop("tdm$mainCommand did not return a list 'result' containing an element 'y'");
  			setwd(oldwd);                                     # restore working dir 

        # append a line with results to result data frame spotConfig$alg.currentResult:
        res <- data.frame(list(Y=result$y
                              ,des[k,setdiff(names(des),c("CONFIG","REPEATS","repeatsLastConfig","STEP","SEED"))]
                              ));
        res <- cbind(res
          					,SEED=seed
         					  ,STEP=theStep
          					,CONFIG=des$CONFIG[k]                  
          					,REP=r
                    );
			  spotConfig$alg.currentResult=rbind(spotConfig$alg.currentResult,res);			
        # (alg.currentResult is initially set to NULL in spotTuner and lhdTuner, see tdmDispatchTuner.r)				
        
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
 	
  	return(spotConfig);            # new, *necessary* !!
}

