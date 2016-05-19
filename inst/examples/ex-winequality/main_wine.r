#
# Template for a data mining process (both with or w/o CV)
# (dataset: winequality, method: Random Forest)
#
# Author: Wolfgang Konen, FHK, July'2015
#
# Example usage:
#       result <- main_connect4();
#
main_wine <- function(opts=NULL,dset=NULL,tset=NULL) {          

    if (is.null(opts)) stop("Argument opts missing");
    opts <- tdmOptsDefaultsSet(opts);  # fill in all opts params which are not yet set (see tdmOptsDefaults.r)
    
    gdObj <- tdmGraAndLogInitialize(opts);     # init graphics and log file

    #===============================================
    # PART 1: READ DATA
    #===============================================
    if (is.null(dset)) {
      cat1(opts,opts$filename,": Read data ...\n")
      dset <- tdmReadData2(opts);
    }
    
    # which variable is response variable:
    response.variable <- "quality" 
    ID.variable <- NULL
    
    # which variables are input variables (in this case all others):
    input.variables <- setdiff(names(dset), c(response.variable,ID.variable))
    
    #===============================================
    # PART 2 - 6
    #===============================================
    result <- tdmClassifyLoop(dset,response.variable,input.variables,opts,tset);

    # print summary output and attach certain columns (here: y,sd.y,dset) to list result:
    result <- tdmClassifySummary(result,opts,dset);

    tdmGraAndLogFinalize(opts,gdObj);      # close graphics and log file
    
    result;  
}

readTrnWine <- function(opts) {
  ds <- read.csv2(file=paste(opts$dir.data, opts$filename, sep=""), dec=".", sep=";", nrow=opts$READ.NROW,header=T);
  for(i in 1:(ncol(ds)-1)){
    ds[,i] <- as.numeric(ds[,i])
  }
  ds[,12] <- as.factor(ds[,12])
  return(ds)
}
