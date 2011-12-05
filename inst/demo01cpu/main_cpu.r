#
# Template for a data mining process (both with or w/o CV)
# (dataset: CPU, method: Random Forest or SVM)
#
# Use "browser()" if you want to look at the variables inside
#
# Author: Wolfgang Konen, FHK, Oct'2009 - Apr'2010
#
main_cpu <- function(opts=NULL) {           
    #tdmPath <- "../tdm";
    #source(paste(tdmPath,"start.tdm.r",sep="/"),local=T);  

    directory <- "./"
    dir.data <- paste(directory, "data/", sep="")
    if (is.null(opts)) {
      opts = tdmOptsDefaultsSet();     # set initial defaults for many elements of opts. See tdmOptsDefaults.r
                                      # for the list of those elements and many explanatory comments                                                                                                         
      opts$dir.output <- paste(directory, "Output/", sep="")
      opts$filename = "cpu.csv"
      opts$SRF.kind = "ndrop"
      opts$SRF.ndrop =  2     # 0..n: how many variables (those with lowest importance) to drop
      opts$OCUT = 600         # cut records with output > OCUT (may be strong outliers, dropping
                              # them makes rmse$test and rmse$OOB faster converge)
      opts$NRUN =  1          # how many runs with different train & test samples (we need about
                              # 25 runs to see that rmse$test and rmse$OOB converge to similar
                              # values)
      opts$NFOLD = 5          # how many cross validation folds
      opts$OUTTRAFO = ""    
      opts$MOD.method="RF";       # ["RF"|"MC.RF"|"SVM"|"NB"]: use [RF| MetaCost-RF| SVM| Naive Bayes] in theClassifyLoop
      opts$RF.ntree = 50
      opts$RF.samp = 1000
      opts$RF.mtry = 3
      opts$SVM.gamma=0.00541;
      opts$SVM.epsilon=0.00527;
      opts$SVM.tolerance=0.00886;
      opts$TST.FRAC = 0.20     # set this fraction of data aside for testing (only for DO.CV=F)
      opts$TST.SEED = NULL    # [NULL] a seed for the random test set selection
      opts$OUTTRAFO = "" 	# "mean.shift"
      opts$fct.postproc <- function(x,opts) { 
        x[x<0] <- 0; 		# clip all negative predictions to value 0	
        x;
      }

      opts$gr.log=T           # if =T: log(x+1)-transform for graphics "true vs. predicted"   
      opts$GD.DEVICE="win"     # ="pdf": all graphics to one multi-page PDF
                              # ="win": all graphics to (several) windows (X11)
      opts$VERBOSE=2;      
    }
    
    opts <- tdmOptsDefaultsFill(opts);  # fill in all opts params which are not yet set (see tdmOptsDefaults.r)
    filename = opts$filename; 
    
    tdmGraAndLogInitialize(opts);     # init graphics and log file
        
    #===============================================
    # PART 1: READ DATA
    #===============================================
    cat1(opts,filename,": Read data ...\n")
    dset <- read.csv2(file=paste(dir.data, filename, sep=""), dec=".",
                      na.string="-1",nrow=opts$READ.NROW)
    # REMARK: when you have a large dataset (e.g. 100000 records), you may use 
    # nrow=100 as long as you experiment with R script and code (during 
    # debugging) in order to speed up things. When your real experiment starts, 
    # set nrow=-1 to fetch all records.
    cat1(opts,filename,":", length(dset[,1]), "records read.\n")

    # which variable is response variable:
    response.variables <- "ERP" 
    ID.variable <- "ID"
    
    #===============================================
    # PART 2a: DATA PREPARATION
    #===============================================
    # special for cpu-dataset: force columns to be numeric (otherwise R thinks 
    # that they are factors and rowSums below does not work)
    for (n in 2:7) {    
        dset[,n] <- as.numeric(dset[,n])  
    }
    # PREPROC: diminuish the skewness of the target variable  
    #dset[,response.variables] <- log(dset[,response.variables]+1) 
    
    # set input variables (everything what is not response.variables and not "ID"):
    input.variables <- setdiff(names(dset), c(response.variables,ID.variable))
    
    #===============================================
    # PART 2b: DATA RECORD SELECTION
    #===============================================
    # disregard records which contain extreme values in response.variable (outliers)
    dset <- dset[dset[,response.variables]<opts$OCUT,] 
    opts$lim = c(min(dset[,response.variables]),max(dset[,response.variables]))
    cat1(opts,filename,":", length(dset[,1]), "records used.\n")

    #===============================================
    # PART 3 - 6
    #===============================================
    result <- tdmRegressLoop(dset,response.variables,input.variables,opts);

    # print summary output and attach certain columns (here: y,sd.y,dset) to list result:
    result <- tdmRegressSummary(result,opts,dset);
    
    tdmGraAndLogFinalize(opts);      # close graphics and log file
    
    result;
    
}                                                   

#result = main_cpu()                            