######################################################################################
#tdmOptsDefaultsSet:
#
#'   Default values for list \code{opts}. Set up and return a list \code{opts} with default settings. 
#' 
#'   For all parameters that do NOT depend on previously def'd elements of \code{opts}.
#'
#' @return a list \code{opts} with defaults set for all options relevant for a DM task, 
#'    containing the following elements
#' 			\item{dir.data}{[./data] where to find data files} 
#' 			\item{dir.Rdata}{[./Rdata] where to find .Rdata files} 
#' 			\item{dir.txt}{[./data] where to find .txt/.csv files} 
#' 			\item{dir.output}{[./Output] where to put output files} 
#' 			\item{filename}{["default.txt"] the task data} 
#' 			\item{filesuffix}{[".txt"] suffix of filename} 
#' 			\item{data.title}{["Default Data"] title for plots} 
#' 			\item{READ.TXT}{[T] =T: read data from .csv and save as .Rdata, =F: read from .Rdata}                                                   
#' 			\item{READ.NROW}{[-1] read this amount of rows or -1 for 'read all rows' (only effective if READ.TXT=T)} 
#' 			\item{READ.TST}{[F] =T: read unseen test data (do this only for the final model and only with TST.kind="col")} 
#' 			\item{TST.kind}{[] .["cv"|"rand"|"col"] see tdmModCreateCVindex in tdmModelingUtils.r. Default is "rand"} 
#' 			\item{...}{[] ... to be continued ...} 
#' 			\item{...}{[] ... to be continued ...} 
#' 			\item{...}{[] ... to be continued ...} 
#' @export
######################################################################################
tdmOptsDefaultsSet <- function() {
      opts = list()

      directory <- "./"  #""
      opts$dir.data <- paste(directory, "data/", sep="")
      opts$dir.txt  <- paste(directory, "data/", sep="")
      opts$dir.Rdata <- paste(directory, "Rdata/", sep="")
      opts$dir.output <- paste(directory, "Output/", sep="")
      opts$filename = "default.txt"
      opts$filesuffix = ".txt"
      opts$data.title <- "Default Data"

      opts$READ.TXT = TRUE    # =T: read data from .csv and save as .Rdata, =F: read from .Rdata
      opts$READ.NROW = -1     # [-1] read this amount of rows or -1 for 'read all rows' (only effective if READ.TXT=T)
      opts$READ.TST = FALSE   # =T: read unseen test data (do this only for the final model and only with TST.kind="col")
                              # and fill column dset[,opts$TST.COL] accordingly (set it to 1 for those test records)
                              # =F: set a part of the train data aside as test data (as prescribed by TST.kind)
      opts$TST.kind <- "rand" # ["cv"|"rand"|"col"] see tdmModCreateCVindex in tdmModelingUtils.r
      opts$TST.COL <- NULL;   # column with train/test/disregard-flag or NULL
      opts$TST.NFOLD =  3     # number of CV-folds (only for TST.kind=="cv")
      opts$TST.FRAC = 0.10    # set this fraction of data aside for testing (only for TST.kind=="rand")
      opts$TST.SEED = NULL    # [NULL] a seed for the random test set selection
      opts$MOD.SEED = NULL    # [NULL] a seed for the random model initialization (if model is non-deterministic)
      
      opts$PRE.PCA = "none"   # ["none"|"linear"|"kernel"] PCA preprocessing: [don't | normal pca (prcomp) | kernel pca (kernlab) ]
                              # (so far we have problems with kernlab in this app)
      opts$PRE.knum = 0;      # [0] if >0 and if PRE.PCA="kernel", take only a subset of PRE.knum records from dset                         
      opts$PRE.npc <- 0;      # [0] if >0: add monomials of degree 2 for the first PRE.npc columns (PCs)

      opts$SRF.kind = "xperc" # ["xperc"|"ndrop"|"nkeep"|"none"] see tdmModSortedRFimport in tdmModelingUtils.r
      opts$SRF.ndrop = 10;    # 0..n: how many variables (those with lowest importance) to drop (only for SRF.kind=="ndrop")
      opts$SRF.ntree=50;      #
      opts$SRF.verbose=2;     #
      opts$SRF.maxS=40;       #
      opts$SRF.minlsi=1;      #
      opts$SRF.XPerc = 0.95;  # 0.0..1.0: how much of the overall importance to keep
      opts$SRF.calc = T       # =T: calculate SRF variable ranking
                              # =F: reload previously calculated and stored variable ranking
                                    
      opts$method="RF";       # ["RF"|"MC.RF"|"SVM"|"NB"]: use [RF| MetaCost-RF| SVM| Naive Bayes] in tdmClassify
                              # ["RF"|"SVM"|"LM"]: use [RF| SVM| linear model] in tdmRegress

      opts$RF.ntree = 500
      opts$RF.samp = 1000
      opts$RF.mtry = NULL
      opts$RF.nodesize = 1
      opts$RF.OOB = T;        # if =T, return OOB-training set error as tuning measure; if =F, return test set error
      opts$RF.cutoff = NULL
                              # [NULL] vote fractions for the n.class classes. The class i with
                              # maximum ratio (% votes)/RF.cutoff[i] wins. If NULL, then each
                              # class gets the cutoff 1/n.class (i.e. majority vote wins)
      opts$SVM.gamma=0.00541;
      opts$SVM.epsilon=0.00527;     # needed only for regression
      opts$SVM.cost=1.0;
      opts$SVM.C=1;                 # needed only for regression
      opts$SVM.tolerance=0.00886;   
      opts$CLASSWT = NULL     # class weights for the n.class classes, e.g.
                              #     c(10,20) for n.class=2         (the higher, the more costly
                              # is a misclassification of that real class). NULL for no weights
      opts$gainmat <- NULL    # if [NULL], opts$gainmat will be set to unit matrix in tdmClassify.r
      opts$rgain.type="rgain" # ["rgain" (def.)|"meanCA"|"minCA"]: the measure result$R_* will contain 
                              # relative gain (i.e. gain/gainmax), mean or minimum class accuracy. 
                              # In each case the goal is to maximize the measure.

      opts$NRUN =  2          # how many runs with different train & test samples  - or -
                              # how many CV-runs, if opts$TST.kind=="cv"
      opts$rep=1;             # the number of the repeat (1,...,spotConfig$max.repeats) in case of repeated evocations from tuner
      opts$DO.POSTPROC = F;
      opts$DO.GRAPHICS=T      # --- deprecated ---
      opts$GRAPHDEV="win"     # ="pdf": all graphics to one multi-page PDF
                              # ="win": all graphics to (several) windows (X11)
                              # ="non": no graphics at all
      opts$GD.RESTART=T       # [T] =T: restart the graphics device (i.e. close all 'old' windows
                              # or re-open multi-page pdf) in each call to tdmClassfiy or tdmRegress, resp.
                              # =F: leave all windows open (suitable for calls from SPOT)
                              # or write more pages in same pdf
      opts$GD.CLOSE=T         # [T] =T: close graphics device "png", "pdf" at the end of main_*.r
                              # (suitable for main_*.r solo) or =F: do not close (suitable for
                              # call from tdmStartSpot, where all windows should remain open)
      opts$VERBOSE=2;

      opts;
}
######################################################################################
# tdmOptsDefaultsFill:
#
#'   Fill the current \code{opts}. Fill the current \code{opts} with further default 
#'   parameters if they are not yet defined. The defaults may depend on previously 
#'   defined elements of \code{opts} (e.g. \code{opts$filename}). 
#'
#' @param opts    the options 
#' @param suffix  the suffix of \code{opts$filename}. If NULL, take opts$filesuffix
#' @return opts   the options  
#' @export
######################################################################################
tdmOptsDefaultsFill <- function(opts,suffix=".csv") {
    filename = opts$filename;
    if (is.null(suffix)) suffix = opts$filesuffix;
    if (length(grep(suffix,filename))==0)
      stop("filename and suffix do not fit to each other: ",filename," ",suffix)

    if (is.null(opts$PDFFILE)) opts$PDFFILE=sub(suffix,"_pic.pdf",filename)
    if (is.null(opts$GD.PNGDIR)) opts$GD.PNGDIR=paste("PNG",sub(suffix,"",filename),"/",sep="");
    if (is.null(opts$LOGFILE)) opts$LOGFILE=sub(suffix,".log",filename)
    if (is.null(opts$EVALFILE)) opts$EVALFILE=sub(suffix,"_eval.csv",filename)      # contains evaluation results allEVAL
    
    if (is.null(opts$SRF.samp)) opts$SRF.samp=min(opts$RF.samp,3000); # new 06/2011
    if (is.null(opts$SRF.cutoff)) opts$SRF.cutoff=opts$RF.cutoff;     # new 01/2011! might change the default behaviour

    if (!is.null(opts$DO.GRAPHICS))
      if (opts$DO.GRAPHICS==F) opts$GRAPHDEV="non";        # DO.GRAPHICS is now 01/2011 deprecated

    opts;
}
