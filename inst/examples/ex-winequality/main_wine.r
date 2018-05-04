#
# Template for a data mining process (both with or w/o CV)
# (dataset: winequality, method: Random Forest)
#
# Example usage: 
#     opts = list(path=".", dir.data="data", filename="winequality-white-train.csv",READ.TrnFn=readTrnWine)
#     opts <- setParams(opts,defaultOpts(),keepNotMatching=T)
#     result <- main_wine(opts)
#
# see also: start_wine.r
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
      dset <- tdmReadDataset(opts);
    }
    
    # which variable is response variable:
    response.variable <- "quality" 
    ID.variable <- "TST.COL"
    
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
  ds <- read.csv2(file=paste(opts$path,opts$dir.data, opts$filename, sep="/"), dec=".", sep=";", nrow=opts$READ.NROW,header=T);
  for(i in 1:(ncol(ds)-1)){
    ds[,i] <- as.numeric(ds[,i])
  }
  ds[,12] <- as.factor(ds[,12])
  return(ds)
}

readTstWine <- function(opts) {
  ds <- read.csv2(file=paste(opts$path,opts$dir.data, opts$filetest, sep="/"), dec=".", sep=";", nrow=opts$READ.NROW,header=T);
  for(i in 1:(ncol(ds)-1)){
    ds[,i] <- as.numeric(ds[,i])
  }
  ds[,12] <- as.factor(ds[,12])
  return(ds)
}

# splitWine: just a helper function for startWine and finalWine which distributes the original
# wine quality data in 80% winequality-white-train.csv and 20% winequality-white-test.csv.
# Change the argument of set.seed, if you want another random distribution.
splitWine <- function(opts) {
  ds <- read.csv2(file=paste(opts$dir.data, "winequality-white-orig.csv", sep=""), dec=".", sep=";", header=T);
  set.seed(4)
  smp = sample(1:nrow(ds),0.2*nrow(ds))
  write.table(ds[ smp,],file=paste(opts$path,opts$dir.data, opts$filetest, sep="/"), dec=".", sep=";", col.names=T, row.names=F, quote=F)
  write.table(ds[-smp,],file=paste(opts$path,opts$dir.data, opts$filename, sep="/"), dec=".", sep=";", col.names=T, row.names=F, quote=F)
}
