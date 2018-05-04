#
# Template for a data mining process (both with or w/o CV)
# (dataset: Sonar, method: Random Forest)
#
# Use "browser()" if you want to look at the variables inside
#
# Author: Wolfgang Konen, FHK, Oct'2009 - Apr'2014
#
# Example usage:
#       result <- main_sonar(opts);
#

#readCmdSonar <- function(filename,opts) {
#  read.csv2(file=paste(opts$path,opts$dir.data, filename, sep="/"), dec=".", sep=",", nrow=opts$READ.NROW,header=FALSE);
#}
readTrnSonar <- function(opts) {
  read.csv2(file=paste(opts$path,opts$dir.data, opts$filename, sep="/"), dec=".", sep=",", nrow=opts$READ.NROW,header=FALSE);
}

main_sonar <- function(opts,dset=NULL,tset=NULL) {          

    opts <- tdmOptsDefaultsSet(opts);  # fill in all opts params which are not yet set (see tdmOptsDefaults.r)
    
    gdObj <- tdmGraAndLogInitialize(opts);     # init graphics and log file

    #===============================================
    # PART 1: READ DATA
    #===============================================
    if (is.null(dset)) {
      cat1(opts,opts$filename,": Read data ...\n")
      dset <- tdmReadDataset(opts);
    }
    names(dset)[61] <- "Class"
    if (!is.null(tset)) names(tset)[61] <- "Class"
    
    # alternative way (but this requires mlbench):
    #require(mlbench); data(Sonar);      # 60 columns V1,...,V60 with input data, 
    #dset <- Sonar;                      # one response column "Class" with levels ["M" (metal) | "R" (rock)] 
    
    # which variable is response variable:
    response.variable <- "Class" 
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

#result = main_sonar() 
