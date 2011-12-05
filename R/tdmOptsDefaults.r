######################################################################################
#tdmOptsDefaultsSet:
#
#'   Default values for list \code{opts}. Set up and return a list \code{opts} with default settings. 
#' 
#'   For better readability, the elements of  \code{opts} are arranged in groups:
#'     \tabular{ll}{
#'      \code{dir.*} \tab  path-related settings  \cr
#'      \code{READ.*} \tab  data-reading-related settings  \cr
#'      \code{TST.*} \tab  resampling-related settings (training and test set, CV)  \cr    
#'      \code{PRE.*} \tab  preprocessing parameters \cr
#'      \code{SRF.*} \tab  several parameters for \code{\link{tdmModSortedRFimport}}   \cr
#'      \code{MOD.*} \tab  general settings for models and model building  \cr
#'      \code{RF.*} \tab  several parameters for model RF (Random Forest)    \cr
#'      \code{SVM.*} \tab  several parameters for model SVM (Support Vector Machines)  \cr
#'      \code{CLS.*} \tab  classification-related settings  \cr
#'      \code{GD.*} \tab  settings for the graphic devices  \cr
#'     }
#'
#'   What is the difference between \code{\link{tdmOptsDefaultsSet}} and \code{\link{tdmOptsDefaultsFill}}? 
#'   \code{tdmOptsDefaultsSet} is for all parameters that do NOT depend on previously def'd elements of \code{opts}.
#'   \code{tdmOptsDefaultsFill} is used to fill in further \code{opts} elements, if not yet defined, depending on 
#'   previous settings (e. g. opts$LOGFILE is derived from opts$filename).
#'
#'   When opts$READ.TST==T, the following things happen: Data are read from opts$filename and from opts$filetest. Both data sets 
#'   are bound together, with a new column opts$TST.COL having '0' for the data from opts$filename and having '1' for the data 
#'   from opts$filetest.  This option is invoked with umode="TST" in \code{\link{unbiasedRun}}.
#'
#' @return a list \code{opts} with defaults set for all options relevant for a DM task, 
#'    containing the following elements
#' 			\item{dir.data}{[./data] where to find data files} 
#' 			\item{dir.Rdata}{[./Rdata] where to find .Rdata files} 
#' 			\item{dir.txt}{[./data] where to find .txt/.csv files} 
#' 			\item{dir.output}{[./Output] where to put output files} 
#' 			\item{filename}{["default.txt"] the task data} 
#' 			\item{filetest}{[NULL] the test data, only relevant for READ.TST=T} 
#' 			\item{data.title}{["Default Data"] title for plots} 
#' 			\item{READ.TXT}{[T] =T: read data from .csv and save as .Rdata, =F: read from .Rdata}                                                   
#' 			\item{READ.NROW}{[-1] read this amount of rows or -1 for 'read all rows'} 
#' 			\item{READ.TST}{[F] =T: read unseen test data from opts$filetest (usually you will do this only for the final model and only with TST.kind="col")} 
#' 			\item{TST.kind}{["cv"|"rand"|"col"] see tdmModCreateCVindex in tdmModelingUtils.r. Default is "rand"} 
#' 			\item{TST.COL}{name of column with train/test/disregard-flag or NULL} 
#' 			\item{TST.NFOLD}{[3] number of CV-folds (only for TST.kind=="cv")} 
#' 			\item{TST.FRAC}{[0.1] set this fraction of data aside for testing (only for TST.kind=="rand")} 
#' 			\item{TST.SEED}{[NULL] a seed for the random test set selection. If NULL, use \code{\link{tdmRandomSeed}}. } 
#' 			\item{CLS.cutoff}{[NULL] vote fractions for the n.class classes. The class i with maximum ratio (\% votes)/CLS.cutoff[i] wins. 
#'                        If NULL, then each class gets the cutoff 1/n.class (i.e. majority vote wins) }
#' 			\item{CLS.CLASSWT}{[NULL] class weights for the n.class classes, e.g. c(10,20) for n.class=2. The higher, the more costly
#'                        is a misclassification of that real class). NULL for equal weights for each class.} 
#' 			\item{CLS.gainmat}{[NULL] (n.class x n.class) gain matrix. If NULL, CLS.gainmat will be set to unit matrix in \code{\link{tdmClassify}} }
#' 			\item{PRE.PCA}{["none" (default)|"linear"] PCA preprocessing: [don't | normal pca (prcomp) ] } 
#' 			\item{PRE.npc}{[0] if >0: add monomials of degree 2 for the first PRE.npc columns (PCs)} 
#' 			\item{SRF.kind}{["xperc" (default) |"ndrop" |"nkeep" |"none" ] the method used for feature selection, see \code{\link{tdmModSortedRFimport}}  } 
#'      \item{SRF.ndrop}{   [0] how many variables to drop (if SRF.kind=="ndrop")  }
#'      \item{SRF.XPerc}{  [0.95] if >=0, keep that importance percentage, starting with the most important variables (if SRF.kind=="xperc")  }
#'      \item{SRF.calc}{   [T] =T: calculate importance & save on SRF.file, =F: load from SRF.file
#'                         (SRF.file = Output/<filename>.SRF.<response.variable>.Rdata) }
#'      \item{SRF.ntree}{  [50] number of RF trees }
#'      \item{SRF.samp}{    sampsize for RF }
#'      \item{SRF.verbose}{ [2] }
#'      \item{SRF.maxS}{    [40] how many variables to show in plot }
#'      \item{SRF.minlsi}{  [1] a lower bound for the length of SRF$input.variables  }
#' 			\item{MOD.SEED}{[NULL] a seed for the random model initialization (if model is non-deterministic). If NULL, use \code{\link{tdmRandomSeed}}. } 
#' 			\item{MOD.method}{["RF" (default) |"MC.RF" |"SVM" |"NB" ]: use [RF | MetaCost-RF | SVM | Naive Bayes ] in \code{\link{tdmClassify}}  \cr
#'                        ["RF" (default) |"SVM" |"LM" ]: use [RF | SVM | linear model ] in \code{\link{tdmRegress}}  } 
#' 			\item{RF.ntree}{[500] } 
#' 			\item{RF.samp}{[1000] } 
#' 			\item{RF.mtry}{[NULL] } 
#' 			\item{RF.nodesize}{[1] } 
#' 			\item{RF.OOB}{[T] if =T, return OOB-training set error as tuning measure; if =F, return test set error } 
#' 			\item{SVM.gamma}{[0.005] } 
#' 			\item{SVM.epsilon}{[0.005] needed only for regression} 
#' 			\item{SVM.cost}{[1.0] } 
#' 			\item{SVM.C}{[1] needed only for regression} 
#' 			\item{SVM.tolerance}{[0.008] } 
#' 			\item{rgain.type}{["rgain" (default) |"meanCA" |"minCA" ] in case of \code{\link{tdmClassify}}: For classification, the measure 
#'                        returned from \code{\link{tdmClassifyLoop}} in \code{result$R_*} is
#'                        [relative gain (i.e. gain/gainmax) | mean class accuracy | minimum class accuracy ]. The goal is to maximize  \code{Rgain}. \cr
#'                        For regression, the goal is to minimize \code{result$R_*} returned from \code{\link{tdmRegress}}. In this case, possible values are 
#'                        \code{rgain.type} = ["rmae" (default) |"rmse" ] which stands for [ relative mean absolute error | root mean squared error ].  } 
#' 			\item{ncopies}{[0] if >0, activate \code{\link{tdmParaBootstrap}} in \code{\link{tdmClassify}}  } 
#' 			\item{DO.POSTPROC}{[F] =T: call the user-defined postprocessing fct opts$fct.postproc after model building and its application to the test set.} 
#'      \item{fct.postproc}{[NULL] a function with signature \code{(pred, dframe, opts)} where \code{pred} is the prediction of the model on the 
#'                        data frame \code{dframe} and \code{opts} is this list. This function may do some postprocessing on \code{pred}  and
#'                        it returns a (potentially modified) \code{pred}. This function will be called in \code{\link{tdmClassify}} if \code{DO.POSTPROC=T}.  }
#' 			\item{GD.DEVICE}{["win"] ="win": all graphics to (several) windows (\code{windows} or \code{X11} in package \code{grDevices}) \cr
#'                        ="pdf": all graphics to one multi-page PDF \cr
#'                        ="png": all graphics in separate PNG files in \code{opts$GD.PNGDIR} \cr
#'                        ="non": no graphics at all \cr
#'                        This concerns the TDMR graphics, not the SPOT (or other tuner) graphics    } 
#' 			\item{GD.RESTART}{[T] =T: restart the graphics device (i.e. close all 'old' windows or re-open 
#'                        multi-page pdf) in each call to \code{\link{tdmClassify}} or \code{\link{tdmRegress}}, resp. \cr
#'                        =F: leave all windows open (suitable for calls from SPOT) or write more pages in same pdf. } 
#' 			\item{GD.CLOSE}{[T] =T: close graphics device "png", "pdf" at the end of main_*.r (suitable for main_*.r solo) or \cr
#'                      =F: do not close (suitable for call from tdmStartSpot, where all windows should remain open)  } 
#' 			\item{NRUN}{[2] how many runs with different train & test samples  - or - how many CV-runs, if \code{opts$TST.kind}="cv"  } 
#' 			\item{VERBOSE}{[2] =2: print much output, =1: less, =0: none} 
#'
#' @seealso  \code{\link{tdmOptsDefaultsFill}}
#' @author Wolfgang Konen, FHK, Mar'2011 - Dec'2011
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
      opts$data.title <- "Default Data"

      opts$READ.TXT = TRUE    # =T: read data from .csv and save as .Rdata, =F: read from .Rdata
      opts$READ.NROW = -1     # [-1] read this amount of rows or -1 for 'read all rows' 
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
                              # (so far we have problems with kernlab in this app --> currently commented out in tdmPreprocUtils.r)
      opts$PRE.knum = 0;      # [0] if >0 and if PRE.PCA="kernel", take only a subset of PRE.knum records from dset                         
      opts$PRE.npc <- 0;      # [0] if >0: add monomials of degree 2 for the first PRE.npc columns (PCs)

      opts$SRF.kind = "xperc" # ["xperc"|"ndrop"|"nkeep"|"none"] see tdmModSortedRFimport in tdmModelingUtils.r
      opts$SRF.ndrop = 10;    # 0..n: how many variables (those with lowest importance) to drop (only for SRF.kind=="ndrop")
      opts$SRF.ntree=50;      #
      opts$SRF.verbose=2;     #
      opts$SRF.maxS=40;       #
      opts$SRF.minlsi=1;      #
      opts$SRF.XPerc = 0.95;  # 0.0..1.0: how much of the overall importance to keep
      opts$SRF.calc = TRUE    # =T: calculate SRF variable ranking
                              # =F: reload previously calculated and stored variable ranking
                                    
      opts$MOD.method="RF";       # ["RF"|"MC.RF"|"SVM"|"NB"]: use [RF| MetaCost-RF| SVM| Naive Bayes] in tdmClassify
                              # ["RF"|"SVM"|"LM"]: use [RF| SVM| linear model] in tdmRegress

      opts$RF.ntree = 500
      opts$RF.samp = 1000
      opts$RF.mtry = NULL
      opts$RF.nodesize = 1
      opts$RF.OOB = TRUE;     # if =T, return OOB-training set error as tuning measure; if =F, return test set error
      opts$RF.p.all=FALSE;
      opts$SVM.gamma=0.005;
      opts$SVM.epsilon=0.005;     # needed only for regression
      opts$SVM.cost=1.0;
      opts$SVM.C=1;                 # needed only for regression
      opts$SVM.tolerance=0.008;   
      opts$CLS.cutoff = NULL  # [NULL] vote fractions for the n.class classes. The class i with
                              # maximum ratio (% votes)/RF.cutoff[i] wins. If NULL, then each
                              # class gets the cutoff 1/n.class (i.e. majority vote wins)
      opts$CLS.CLASSWT = NULL # class weights for the n.class classes, e.g.
                              #     c(10,20) for n.class=2         (the higher, the more costly
                              # is a misclassification of that real class). NULL for no weights
      opts$CLS.gainmat <- NULL# if [NULL], opts$CLS.gainmat will be set to unit matrix in \code{\link{tdmClassify}}
      opts$rgain.type="rgain" # ["rgain" (def.)|"meanCA"|"minCA"] for tdmClassify: the measure result$R_* will contain 
                              # relative gain (i.e. gain/gainmax), mean or minimum class accuracy. 
                              # In each case the goal is to maximize the measure.
                              # ["rmae" (default) |"rmse" ] for regression (tdmRegress). Here the goal is to minimize result$R_*.
      opts$ncopies = 0;       # if >0, activate tdmParaBootstrap in tdmClassify                       

      opts$DO.POSTPROC = FALSE;
      opts$fct.postproc=NULL;
      opts$DO.GRAPHICS=T      # --- deprecated, use opts$GD.DEVICE="non" ---
      opts$GD.DEVICE="win"    # ="pdf": all graphics to one multi-page PDF
                              # ="win": all graphics to (several) windows (X11)
                              # ="non": no graphics at all
                              # This concerns the TDMR graphics, not the SPOT graphics
      opts$GD.RESTART=TRUE    # [T] =T: restart the graphics device (i.e. close all 'old' windows
                              # or re-open multi-page pdf) in each call to tdmClassfiy or tdmRegress, resp.
                              # =F: leave all windows open (suitable for calls from SPOT)
                              # or write more pages in same pdf
      opts$GD.CLOSE=TRUE      # [T] =T: close graphics device "png", "pdf" at the end of main_*.r
                              # (suitable for main_*.r solo) or =F: do not close (suitable for
                              # call from tdmStartSpot, where all windows should remain open)
      opts$NRUN =  2          # how many runs with different train & test samples  - or -
                              # how many CV-runs, if opts$TST.kind=="cv"
      opts$rep=1;             # the number of the repeat (1,...,spotConfig$max.repeats) in case of repeated evocations from tuner
                              # (needed only internally as private storage for the RNG, see tdmClassify.r
      opts$APPLY_TIME=FALSE;                              
      opts$test2.string <- "default cutoff";
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
#' @param suffix  the suffix of \code{opts$filename}. If NULL, take opts$filesuffix (which, if also NUL, is inferred beforehand from opts$filename)
#' @return \code{opts},  the extended options, where additional elements, if they are not yet def'd,  are set as: 
#' 			\item{filesuffix}{the suffix of \code{opts$filename}, e.g. \code{".csv"} } 
#' 			\item{TST.COL}{["TST.COL"] } 
#' 			\item{PDFFILE}{["*_pic.pdf"] file for multipage graphics in case \code{opts$GD.DEVICE}="pdf" } 
#' 			\item{GD.PNGDIR}{["PNG*"] directory for .png files in case \code{opts$GD.DEVICE}="png" } 
#' 			\item{LOGFILE}{["*.log"] where to log the output } 
#' 			\item{EVALFILE}{["*_eval.csv"] file with evaluation results allEVAL } 
#' 			\item{SRF.samp}{sample size for SRF, derived from \code{RF.samp} } 
#' 			\item{SRF.cutoff}{[CLS.cutoff] } 
#'      \item{rgain.string}{ one out of c("RGain","MeanCA","MinCA","RMAE","RMSE"), depending on \code{opts$rgain.type} }
#'
#' Here * is the stripped part of \code{opts$filename} (w/o suffix).
#'
#' All files and directories in the above settings are relative to dir  \code{opts$dir.output}.
#'
#' @seealso  \code{\link{tdmOptsDefaultsSet}}
#' @author Wolfgang Konen, FHK, Mar'2011 - Dec'2011
#' @export
######################################################################################
tdmOptsDefaultsFill <- function(opts,suffix=NULL) {
    if (is.null(opts)) opts = tdmOptsDefaultsSet();
    
    filename = opts$filename; 

    if (is.null(opts$filesuffix)) {
      opts$filesuffix = tail(unlist(strsplit(opts$filename,".",fixed=T)),1);
      opts$filesuffix = paste(".",opts$filesuffix,sep="");
    }
    if (is.null(suffix)) suffix = opts$filesuffix;
    if (length(grep(suffix,filename))==0)
      stop("filename and suffix do not fit to each other: ",filename," ",suffix)

    if (is.null(opts$TST.COL)) opts$TST.COL="TST.COL";
    if (is.null(opts$PDFFILE)) opts$PDFFILE=sub(suffix,"_pic.pdf",filename)
    if (is.null(opts$GD.PNGDIR)) opts$GD.PNGDIR=paste("PNG",sub(suffix,"",filename),"/",sep="");
    if (is.null(opts$LOGFILE)) opts$LOGFILE=sub(suffix,".log",filename)
    if (is.null(opts$EVALFILE)) opts$EVALFILE=sub(suffix,"_eval.csv",filename)      # contains evaluation results allEVAL
    
    if (is.null(opts$SRF.samp)) opts$SRF.samp=min(opts$RF.samp,3000);  # new 06/2011
    if (is.null(opts$SRF.cutoff)) opts$SRF.cutoff=opts$CLS.cutoff;     # new 01/2011! might change the default behaviour
    if (is.null(opts$rgain.string)) {
      rgainTypeVals = c("rgain","meanCA","minCA","rmae","rmse");
      rgainStringVals=c("RGain","MeanCA","MinCA","RMAE","RMSE");
      opts$rgain.string = rgainStringVals[which(opts$rgain.type==rgainTypeVals)];
    }

    if (!is.null(opts$DO.GRAPHICS))
      if (opts$DO.GRAPHICS==F) opts$GD.DEVICE="non";        # DO.GRAPHICS is now (01/2011) deprecated

    if (!is.null(opts$GRAPHDEV) && is.null(opts$GD.DEVICE))
      opts$GD.DEVICE=opts$GRAPHDEV;                         # opts$GRAPHDEV is now (12/2011) deprecated

    # code which was previously in tdmClassify. Now we put it here and call tdmOptsDefaultsFill from tdmClassify
    # (cleaner code, less places where opts-values are set)
    #
    if (is.null(opts$MOD.method)) opts$MOD.method="RF";
    if (is.null(opts$SRF.kind)) opts$SRF.kind="xperc";
    if (is.null(opts$RF.p.all)) opts$RF.p.all=FALSE;
    if (is.null(opts$RF.OOB)) opts$RF.OOB=TRUE;
    if (is.null(opts$APPLY_TIME)) opts$APPLY_TIME=FALSE;
    if (is.null(opts$DO.POSTPROC)) opts$DO.POSTPROC=FALSE;  
    if (is.null(opts$GD.RESTART)) opts$GD.RESTART=TRUE;  
    if (is.null(opts$VERBOSE)) opts$VERBOSE=2;
    if (is.null(opts$test2.string)) opts$test2.string <- "default cutoff";
    
    # code which was previously in tdmModSortedRFimport. Now we put it here and call tdmOptsDefaultsFill from tdmModSortedRFimport
    # (cleaner code, less places where opts-values are set)
    #
    if (is.null(opts$SRF.XPerc)) opts$SRF.XPerc=0.95;
    if (is.null(opts$SRF.calc)) opts$SRF.calc=TRUE;
    if (is.null(opts$SRF.ntree)) opts$SRF.ntree=50;
    if (is.null(opts$SRF.verbose)) opts$SRF.verbose=2;
    if (is.null(opts$SRF.maxS)) opts$SRF.maxS=40;
    if (is.null(opts$SRF.minlsi)) opts$SRF.minlsi=1;
    if (is.null(opts$SRF.method)) opts$SRF.method="RFimp";

    opts;
}