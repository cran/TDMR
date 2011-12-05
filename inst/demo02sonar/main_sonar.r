#
# Template for a data mining process (both with or w/o CV)
# (dataset: Sonar, method: Random Forest)
#
# Example usage:
#       result <- main_sonar();
# Use "browser()" if you want to look at the variables inside
#
# Author: Wolfgang Konen, FHK, Sep'2010 - Dec'2010
#
main_sonar <- function(opts=NULL) {          

    if (is.null(opts)) {
      opts = tdmOptsDefaultsSet();    # set initial defaults for many elements of opts. See tdmOptsDefaults.r
                                      # for the list of those elements and many explanatory comments                                                                                                         
      opts$filename = "sonar.txt"
      opts$filesuffix = ".txt"
      opts$data.title <- "Sonar Data"

      opts$SRF.kind = "xperc"         # no variable ranking, no variable selection
      opts$MOD.method="RF";               # ["RF"|"MC.RF"|"SVM"|"NB"]: use [RF| MetaCost-RF| SVM| Naive Bayes] in tdmClassifyLoop
      opts$PRE.PCA = "linear"         # ["none"|"linear"|"kernel"] PCA preprocessing
      opts$PRE.npc = 7      # tmp
      
      # Activate the following four lines to run the RF.default-(main_sonar.r)-experiment in Benchmark-Datasets.doc:
      opts$GD.DEVICE = "pdf";
      opts$TST.kind <- "cv"           # ["cv"|"rand"|"col"] see tdmModCreateCVindex in tdmModelingUtils.r
      opts$TST.NFOLD =  3             # number of CV-folds (only for TST.kind=="cv")
      opts$NRUN =  1                  # how many CV-runs, if opts$TST.kind <- "cv"
    }
    opts <- tdmOptsDefaultsFill(opts);  # fill in all opts params which are not yet set (see tdmOptsDefaults.r)
    
    tdmGraAndLogInitialize(opts);     # init graphics and log file

    #===============================================
    # PART 1: READ DATA
    #===============================================
    cat1(opts,opts$filename,": Read data ...\n")
    dset <- read.csv2(file=paste(opts$dir.data, opts$filename, sep=""), dec=".", sep=",",header=FALSE)
    names(dset)[61] <- "Class"

    # alternative way (but this requires mlbench):
    #require(mlbench); data(Sonar);      # 60 columns V1,...,V60 with input data, 
    #dset <- Sonar;                      # one response column "Class" with levels ["M" (metal) | "R" (rock)] 
    
    # which variable is response variable:
    response.variable <- "Class" 
    ID.variable <- NULL
    
    # which variables are input variables (in this case all others):
    input.variables <- setdiff(names(dset), c(response.variable,ID.variable))

    if (opts$PRE.npc>0 | opts$PRE.PCA!="none") {
      # a) do PCA on the numeric variables, if opts$PRE.PCA!="none"
      # b) add monomials of degree 2 for the first opts$PRE.npc numeric variables
      numeric.variables=input.variables;
      pca <- tdmPrePCA(dset,numeric.variables,opts);   # see tdmPreprocUtils.r
      dset <- pca$dset;
      input.variables <- pca$numeric.variables;
    }

    #===============================================
    # PART 2 - 6
    #===============================================
    result <- tdmClassifyLoop(dset,response.variable,input.variables,opts);

    # print summary output and attach certain columns (here: y,sd.y,dset) to list result:
    result <- tdmClassifySummary(result,opts,dset);

    tdmGraAndLogFinalize(opts);      # close graphics and log file
    
    result;  
}

#result = main_sonar() 
