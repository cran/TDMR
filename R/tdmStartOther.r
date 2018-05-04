######################################################################################
# tdmStartOther: 
#     returns the fitness (mean(yres)) and saves other
#     diagnostic results in envT$spotConfig, envT$res, envT$bst
# 
# Known callers: cmaesTuner, powellTuner, bfgsTuner in tdmDispatchTuner.r
#
# Author: Wolfgang Konen, FHK, 2011 - 2013
#
tdmStartOther <-  function(x,tdm,envT,dataObj,opts=NULL) {
  if (!is.list(tdm)) stop("tdm must be a list!");
  if (!is.environment(envT)) stop("envT must be an environment!");
  
#  function(x, opts=NULL) {
    
    #TODO PK
    if (is.list(x)) {
      # Transform list to vector (for MIES integration)
# -- WK -- this has problems with cma_jTuner, which also puts in a list       
#      for (i in length(x)){
#        if (length(x[[i]])>1){
#          stop("Error: Vector parameters are not yet supported in TDMR!")
#        }
#      }
      x = as.vector(x)
      #x = t(x)
    } 
    #browser()

  	if (exists(".Random.seed")) SAVESEED<-.Random.seed	   #save the Random Number Generator RNG status
    if (is.null(opts)) opts <- envT$spotConfig$opts;
    if (!is.null(tdm$constraintFnc)) x <- tdm$constraintFnc(x,tdm);        # needed for Powell tuner
    dset <- NULL;
    if(!is.null(dataObj)) dset<-dsetTrnVa(dataObj,envT$nExp);
    # If dset is not NULL, this has an effect on mainCommand (see below) which contains "...,dset=dset".
    # If dset is NULL, the reading of the data is deferred to main_TASK (deprecated).
  
    des <- tdmMapDesFromX(t(x),envT);
    
    k=1;                                # only one design point
    yres=NULL;
  	for (r in 1:des$REPEATS[k]){
      opts$rep <- r;
    	opts <- tdmMapDesApply(des,opts,k,envT$spotConfig,tdm);
    			
      if (!is.null(des$STEP))	theStep <- des$STEP[k];
			opts$ALG.SEED <- des$SEED[k]+r;				# now used in tdmClassify, tdmClassifyLoop
  		
  		cat(sprintf("Config: %5d,   Repeat: %5d\n",des$CONFIG[k],r))

      result <- eval(call(tdm$mainFunc,opts,dset=dset))            # tdm$mainFunc has to return result$y
      if (is.null(result$y)) stop("tdm$mainFunc did not return a list 'result' containing an element 'y'");

      # write a line with results to data frame envT$spotConfig$alg.currentResult :
      pNames=row.names(envT$spotConfig$alg.roi);
      res <- data.frame(list(result$y,des[k,pNames]));  # bug fix 05/12: this way it works for length(pNames)==1 and for >1
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
  	}	# for (r, REPEATS)			

    if (length(des$CONFIG)>0) { #   %% envT$spotConfig$seq.design.new.size==0) {
    #if (des$CONFIG %% envT$spotConfig$seq.design.new.size==0) {
      # after finishing as many tdmStartOther-calls as there are design points 
      # in a SPOT iteration, compute the so far best solution (merge repeats), write 
      # a line to envT$bst and plot it (the same way as in SPOT):
  	  envT$spotConfig <- tdmPrepareData(envT$spotConfig);	# appends the best solution to envT$spotConfig$alg.currentBest
  	  #mergedData <- spotPrepareData(envT$spotConfig)           ### old version TDMR 1.0
  	  #envT$spotConfig <- spotWriteBest(mergedData, envT$spotConfig);	
  	  #if (envT$spotConfig$io.verbosity>1) spotPlotBst(envT$spotConfig);  
  		envT$bst <- envT$spotConfig$alg.currentBest;  
    }

   	if (exists("SAVESEED")) assign(".Random.seed", SAVESEED, envir=globalenv()); 		#load the saved RNG status
    
    mean(yres);     # mean over all repeats
    
} # tdmStartOther


######################################################################################
# tdmStartCma_j:           
#     returns the fitness (mean(yres)) and saves other
#     diagnostic results in envT$spotConfig, envT$res, envT$bst
# 
# Known callers: cma_jTuner  (via cma_jInternRJava)
#
# Author: Wolfgang Konen, THK, 2013
#
tdmStartCma_j <- function(x,tdm,envT,dataObj) {
    if (!is.list(tdm)) stop("tdm must be a list!");
    if (!is.environment(envT)) stop("envT must be an environment!");
    if (is.null(envT$spotConfig$opts)) stop("Error: envT$spotConfig does not contain an element 'opts'");
    if (is.null(envT$nExp)) envT$nExp <- 1;
    
  	if (exists(".Random.seed")) SAVESEED<-.Random.seed	   #save the Random Number Generator RNG status
    opts <- envT$spotConfig$opts;
    dset <- NULL;
    if(!is.null(dataObj)) dset<-dsetTrnVa(dataObj,envT$nExp);
    # If dset is not NULL, this has an effect on mainCommand (see below) which contains "...,dset=dset".
    # If dset is NULL, the reading of the data is deferred to main_TASK (deprecated).
  
    des <- tdmMapDesFromX(t(x),envT);    	
    
    k=1;                                # only one design point
    yres=NULL;
  	for (r in 1:des$REPEATS[k]){
      opts$rep <- r;
    	opts <- tdmMapDesApply(des,opts,k,envT$spotConfig,tdm);
    			
      if (!is.null(des$STEP))	theStep <- des$STEP[k];
			opts$ALG.SEED <- des$SEED[k]+r;				# now used in tdmClassify, tdmClassifyLoop
  		
  		cat(sprintf("Config: %5d,   Repeat: %5d\n",des$CONFIG[k],r))

  		result <- eval(call(tdm$mainFunc,opts,dset=dset))            # tdm$mainFunc has to return result$y
  		if (is.null(result$y)) stop("tdm$mainFunc did not return a list 'result' containing an element 'y'");

      # write a line with results to the result file resFileName:
      pNames=row.names(envT$spotConfig$alg.roi);
      res <- data.frame(list(result$y,des[k,pNames]));  # bug fix 05/12: this way it works for length(pNames)==1 and for >1
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

  	}	# for (r, REPEATS)			

  	if (length(des$CONFIG)>0) { #   %% envT$spotConfig$seq.design.new.size==0) {
  	#if (des$CONFIG %% envT$spotConfig$seq.design.new.size==0) {
  	  # after finishing as many tdmStartOther-calls as there are design points 
      # in a SPOT iteration, compute the so far best solution (merge repeats), write 
      # a line to envT$bst and plot it (the same way as in SPOT):
  	  envT$spotConfig <- tdmPrepareData(envT$spotConfig);	# appends the best solution to envT$spotConfig$alg.currentBest
  	  #mergedData <- spotPrepareData(envT$spotConfig)           ### old version TDMR 1.0
      #envT$spotConfig <- spotWriteBest(mergedData, envT$spotConfig);	
  		#if (envT$spotConfig$io.verbosity>1) spotPlotBst(envT$spotConfig);  
  		envT$bst <- envT$spotConfig$alg.currentBest;  
    }
    #print(mean(yres));
    
   	if (exists("SAVESEED")) assign(".Random.seed", SAVESEED, envir=globalenv()); 		#load the saved RNG status
    
    mean(yres);     # return the fitness = mean over all repeats
    
}  # tdmStartCma_j

