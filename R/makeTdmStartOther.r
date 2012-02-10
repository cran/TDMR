######################################################################################
# makeTdmStartOther: factory method to generate the function tdmStartOther.
#   The function object tdmStartOther is def'd at a point in time where tdm and envT 
#   already exist. In this way, tdmStartOther can use these variables although
#   they do not appear in the argument list of tdmStartOther
# 
# Known callers: cmaesTuner, powellTuner, bfgsTuner in tdmDispatchTuner.r
#
# Author: Wolfgang Konen, FHK, May'2011 - May'2011
#
######################################################################################
makeTdmStartOther <- function(tdm,envT,dataObj) {
  if (!is.list(tdm)) stop("tdm must be a list!");
  if (!is.environment(envT)) stop("envT must be an environment!");
  
  function(x,opts=NULL) {
  	SAVESEED<-.Random.seed	#save the Random Number Generator RNG status
    if (is.null(opts)) opts <- envT$spotConfig$opts;
    if (!is.null(tdm$constraintFnc)) x <- tdm$constraintFnc(x,tdm);
    dset <- ifelse(is.null(dataObj),NULL, dsetTrnVa(dataObj));
    # If dset is not NULL, this has an effect on tdm$mainCommand which contains "...,dset=dset".
	# If dset is NULL, the reading of the data is deferred to main_TASK.

  
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
    des <- tdmMapCutoff(des,k,envT$spotConfig);    # enforce CUTOFF parameter constraint if CUTOFF2[,3,4] appears in .des-file
  	for (r in 1:des$REPEATS[k]){
      opts$rep <- r;
    	opts <- tdmMapDesApply(des,opts,k,envT,tdm);
    			
      if (!is.null(des$STEP))	theStep <- des$STEP[k];
			opts$ALG.SEED <- des$SEED[k]+r;				# now used in tdmClassify, tdmClassifyLoop
  		
  		cat(sprintf("Config: %5d,   Repeat: %5d\n",des$CONFIG[k],r))

      oldwd = getwd();                                             # save working dir
  		if (!is.null(tdm$mainFile)) setwd(dirname(tdm$mainFile));    # optional change working dir 
  		result = NULL; 
      eval(parse(text=tdm$mainCommand));                # execute the command given in text string tdm$mainCommand, which has to return result$y
      if (is.null(result$y)) stop("tdm$mainCommand did not return a list 'result' containing an element 'y'");
  		setwd(oldwd);                                                # restore working dir 
  		
      # write a line with results to the result file resFileName:
      res <- data.frame(list(Y=result$y
                            ,des[k,setdiff(names(des),c("CONFIG","REPEATS","repeatsLastConfig","STEP","SEED"))]
                            ));
      res <- cbind(res
        					,SEED=opts$ALG.SEED
       					  ,STEP=theStep
        					,CONFIG=des$CONFIG[k]                  
        					,REP=r
                  );
      yres[r] = res$Y;
                        
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
  		spotPlotBst(envT$spotConfig);  
  		envT$bst <- envT$spotConfig$alg.currentBest;  
  		#browser()
    }
    #print(mean(yres));
    
   	assign(".Random.seed", SAVESEED, envir=globalenv()); 		#load the saved RNG status
    
    mean(yres);     # mean over all repeats
    
  }
}
