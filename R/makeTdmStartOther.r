######################################################################################
# makeTdmStartOther: factory method to generate the function tdmStartOther.
#   The function object tdmStartOther is def'd at a point in time where tdm and envT 
#   already exist. In this way, tdmStartOther can use these variables although
#   they do not appear in the argument list of tdmStartOther
# 
# Known callers: cmaesTuner, cma_jTuner (via Java program), powellTuner, bfgsTuner in tdmDispatchTuner.r
#
# Author: Wolfgang Konen, FHK, May'2011 - May'2012
#
makeTdmStartOther <- function(tdm,envT,dataObj) {
  if (!is.list(tdm)) stop("tdm must be a list!");
  if (!is.environment(envT)) stop("envT must be an environment!");
  
  function(x,opts=NULL) {
    
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
    if (!is.null(tdm$constraintFnc)) x <- tdm$constraintFnc(x,tdm);
    dset <- NULL;
    if(!is.null(dataObj)) dset<-dsetTrnVa(dataObj);
    # If dset is not NULL, this has an effect on mainCommand (see below) which contains "...,dset=dset".
    # If dset is NULL, the reading of the data is deferred to main_TASK (deprecated).
  
    # put parameter vector x in a one-row data frame and attach param names from .roi file:
    des <- as.data.frame(t(x));
    names(des) <- rownames(envT$spotConfig$alg.roi);
    # round INT columns of data frame des and print its summary (see tdmMapDesign.r):	
  	des <- tdmMapDesInt(des,printSummary=F,envT$spotConfig);     
    
    if (!is.null(envT$res)) {
      des$CONFIG = max(envT$res$CONFIG)+1;
    } else des$CONFIG=1;
    if (!is.null(envT$bst)) {
      des$STEP = nrow(envT$bst);
    } else des$STEP=0;
  
  	des$REPEATS=envT$spotConfig$seq.design.maxRepeats;
  	des$SEED=envT$spotConfig$alg.seed;              # dummy
    if (is.null(des$repeatsLastConfig)) des$repeatsLastConfig=1; # dummy  
      	
    
    k=1;                                # only one design point
    yres=NULL;
    #des <- tdmMapCutoff(des,k,envT$spotConfig);    # enforce CUTOFF parameter constraint if CUTOFF2[,3,4] appears in .des-file
  	for (r in 1:des$REPEATS[k]){
      opts$rep <- r;
    	opts <- tdmMapDesApply(des,opts,k,envT$spotConfig,tdm);
    			
      if (!is.null(des$STEP))	theStep <- des$STEP[k];
			opts$ALG.SEED <- des$SEED[k]+r;				# now used in tdmClassify, tdmClassifyLoop
  		
  		cat(sprintf("Config: %5d,   Repeat: %5d\n",des$CONFIG[k],r))

      oldwd = getwd();                                             # save working dir
  		if (!is.null(tdm$mainFile)) setwd(dirname(tdm$mainFile));    # optional change working dir 
  		result = NULL; 	
  		mainCommand <- paste("result <- ", tdm$mainFunc,"(opts,dset=dset)",sep=" ");
      eval(parse(text=mainCommand));                # execute the command given in text string mainCommand, which has to return result$y
      if (is.null(result$y)) stop("tdm$Func did not return a list 'result' containing an element 'y'");
  		setwd(oldwd);                                                # restore working dir 
  		
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
      if (tdm$fileMode) {                  
    		colNames = ifelse(file.exists(tdm$resFile),FALSE,TRUE);			
    		write.table(res
    				, file = tdm$resFile
    				, row.names = FALSE
    				, col.names = colNames
    				, sep = " "              
    				, append = !colNames            
    				, quote = FALSE                    
    		);			
  		}
  	}	# for (r, REPEATS)			

  	if (des$CONFIG %% envT$spotConfig$seq.design.new.size==0) {
      # after finishing as many tdmStartOther-calls as there are design points 
      # in a SPOT iteration, compute the so far best solution (merge repeats), write 
      # a line to .bst file and plot it (the same way as in SPOT):
    	mergedData <- spotPrepareData(envT$spotConfig)
      envT$spotConfig <- spotWriteBest(mergedData, envT$spotConfig);	# appends the best solution to envT$spotConfig$alg.currentBest
      envT$spotConfig$alg.currentResult <- res       # needed by spotPlotBst for column STEP
  		spotPlotBst(envT$spotConfig);  
  		envT$bst <- envT$spotConfig$alg.currentBest;  
  		#browser()
    }
    #print(mean(yres));
    
   	if (exists("SAVESEED")) assign(".Random.seed", SAVESEED, envir=globalenv()); 		#load the saved RNG status
    
    mean(yres);     # mean over all repeats
    
  }
}
