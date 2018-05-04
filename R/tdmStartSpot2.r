######################################################################################
# tdmStartSpot2: 
#
#'    Function called by SPOT 2.0 tuner 
#'
#'    This function is called when \code{tdm$tuneMethod} = "spot" or "lhd".
#'     
#' @param x   a matrix with design points (one point per row)
#' @return \code{y}, a 1-column matrix, same number of rows as \code{x} 
#'     returns the fitness and saves other
#'     diagnostic results in envT$spotConfig, envT$res, envT$bst
#'
#' @author Wolfgang Konen, THK, 2018
#' @export
#' @keywords internal
#'  
# Known callers: spotTuner in tdmDispatchTuner.r
#
# Details: spot will hand over the whole initial design as one matrix 
# (nrow = designControl$size*designControl$replicates)
# and every model design point as a matrix with control$replicates identical rows.
tdmStartSpot2 <-  function(x,tdm,envT,dataObj,opts=NULL) {
  # tdmMapDesFromX <- function(x,envT) {    
  #   # put parameter vector x in a one-row data frame and attach param names from .roi file:
  #   #des <- as.data.frame(t(x));
  #   des <- as.data.frame(x);
  #   names(des) <- rownames(envT$spotConfig$alg.roi);
  #   # round INT columns of data frame des and print its summary (see tdmMapDesign.r):	
  #   des <- tdmMapDesInt(des,printSummary=F,envT$spotConfig);     
  #   
  #   if (!is.null(envT$res)) {
  #     des$CONFIG = max(envT$res$CONFIG)+1;
  #   } else des$CONFIG=1;
  #   if (!is.null(envT$bst)) {
  #     des$STEP = nrow(envT$bst);
  #   } else des$STEP=0;
  #   
  #   des$REPEATS=envT$spotConfig$replicates;
  #   des$SEED=envT$spotConfig$alg.seed;              # dummy
  #   if (is.null(des$repeatsLastConfig)) des$repeatsLastConfig=1; # dummy  
  #   des;
  # }  	
  
  if (!is.list(tdm)) stop("tdm must be a list!");
  if (!is.environment(envT)) stop("envT must be an environment!");

    
  	if (exists(".Random.seed")) SAVESEED<-.Random.seed	   #save the Random Number Generator RNG status
    if (is.null(opts)) opts <- envT$spotConfig$opts;
    dset <- NULL;
    if(!is.null(dataObj)) dset<-dsetTrnVa(dataObj,envT$nExp);
    # If dset is not NULL, this has an effect on mainCommand (see below) which contains "...,dset=dset".
    # If dset is NULL, the reading of the data is deferred to main_TASK (deprecated).
  
    des <- tdmMapDesFromX(x,envT);

    k=1;                                # only one design point
    yres=NULL;
  	for (r in 1:nrow(x)){
  	  opts$rep <- r;      # tdmClassifyLoop needs opts$rep: to have for each repeat another seed
    	opts <- tdmMapDesApply(des,opts,r,envT$spotConfig,tdm);
    			
      if (!is.null(des$STEP))	theStep <- des$STEP[r];
			opts$ALG.SEED <- des$SEED[r] #+r;				# now used in tdmClassify, tdmClassifyLoop
  		
  		cat(sprintf("Config: %5d,   Repeat: %5d\n",des$CONFIG[k],r))

  		result <- eval(call(tdm$mainFunc,opts,dset=dset))            # tdm$mainFunc has to return result$y
  		if (is.null(result$y)) stop("tdm$mainFunc did not return a list 'result' containing an element 'y'");

      # write a line with results to data frame envT$spotConfig$alg.currentResult :
      pNames=row.names(envT$spotConfig$alg.roi);
      res <- data.frame(list(result$y,des[r,pNames]));  # bug fix 05/12: this way it works for length(pNames)==1 and for >1
      names(res)<-c("Y",pNames);                        #
      res <- cbind(res
        					,SEED=opts$ALG.SEED
       					  ,STEP=theStep
        					,CONFIG=des$CONFIG[k]                  
        					,REP=r
                  );
      yres[r] = res$Y;
 
		  envT$spotConfig$alg.currentResult=rbind(envT$spotConfig$alg.currentResult,res);			     # NEW!! 05/2012
      # (alg.currentResult is initially set to NULL for each tuner, see tdmDispatchTuner.r)				
                        
      envT$res = rbind(envT$res,res);
  		# envT$bst = rbind(envT$bst,min(c(envT$bst,res$Y)))  # WRONG
  	}	# for (r, rows of x)			

  # 	if (length(des$CONFIG)>0) { # %% envT$spotConfig$seq.design.new.size==0) {
      # after finishing the for-loop, we have in any case all replicates for the current design
      # point completed. We merge over identical x-rows with operator mean, to get the best-replicate
      # solution, write a line to envT$bst:
  	  envT$spotConfig <- tdmPrepareData(envT$spotConfig);	# appends the best solution to envT$spotConfig$alg.currentBest
  	  envT$bst <- envT$spotConfig$alg.currentBest;
  	  #mergedData <- spotPrepareData(envT$spotConfig)           ### old version TDMR 1.0
  	  #envT$spotConfig <- spotWriteBest(mergedData, envT$spotConfig);
  	  ### and plot it (the same way as in SPOT)
  	  #if (envT$spotConfig$io.verbosity>1) spotPlotBst(envT$spotConfig);
  #   }

   	if (exists("SAVESEED")) assign(".Random.seed", SAVESEED, envir=globalenv()); 		#load the saved RNG status
    
    yres <- as.matrix(yres);

} # tdmStartSpot2




