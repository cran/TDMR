\name{tdmOptsDefaultsSet}
\alias{tdmOptsDefaultsSet}
\title{Default values for list opts.}
\description{Default values for list \code{opts}. Set up and return a list \code{opts} with default settings.}
\details{For better readability, the elements of  \code{opts} are arranged in groups:
\tabular{ll}{
\code{dir.*} \tab  path-related settings  \cr
\code{READ.*} \tab  data-reading-related settings  \cr
\code{TST.*} \tab  resampling-related settings (training and test set, CV)  \cr    
\code{PRE.*} \tab  preprocessing parameters \cr
\code{SRF.*} \tab  several parameters for \code{\link{tdmModSortedRFimport}}   \cr
\code{MOD.*} \tab  general settings for models and model building  \cr
\code{RF.*} \tab  several parameters for model RF (Random Forest)    \cr
\code{SVM.*} \tab  several parameters for model SVM (Support Vector Machines)  \cr
\code{CLS.*} \tab  classification-related settings  \cr
\code{GD.*} \tab  settings for the graphic devices  \cr
}

What is the difference between \code{\link{tdmOptsDefaultsSet}} and \code{\link{tdmOptsDefaultsFill}}? 
\code{tdmOptsDefaultsSet} is for all parameters that do NOT depend on previously def'd elements of \code{opts}.
\code{tdmOptsDefaultsFill} is used to fill in further \code{opts} elements, if not yet defined, depending on 
previous settings (e. g. opts$LOGFILE is derived from opts$filename).

When opts$READ.TST==T, the following things happen: Data are read from opts$filename and from opts$filetest. Both data sets 
are bound together, with a new column opts$TST.COL having '0' for the data from opts$filename and having '1' for the data 
from opts$filetest.  This option is invoked with umode="TST" in \code{\link{unbiasedRun}}.}
\value{a list \code{opts} with defaults set for all options relevant for a DM task, 
containing the following elements
\item{dir.data}{[./data] where to find data files} 
\item{dir.Rdata}{[./Rdata] where to find .Rdata files} 
\item{dir.txt}{[./data] where to find .txt/.csv files} 
\item{dir.output}{[./Output] where to put output files} 
\item{filename}{["default.txt"] the task data} 
\item{filetest}{[NULL] the test data, only relevant for READ.TST=T} 
\item{data.title}{["Default Data"] title for plots} 
\item{READ.TXT}{[T] =T: read data from .csv and save as .Rdata, =F: read from .Rdata}                                                   
\item{READ.NROW}{[-1] read this amount of rows or -1 for 'read all rows'} 
\item{READ.TST}{[F] =T: read unseen test data from opts$filetest (usually you will do this only for the final model and only with TST.kind="col")} 
\item{TST.kind}{["cv"|"rand"|"col"] see tdmModCreateCVindex in tdmModelingUtils.r. Default is "rand"} 
\item{TST.COL}{name of column with train/test/disregard-flag or NULL} 
\item{TST.NFOLD}{[3] number of CV-folds (only for TST.kind=="cv")} 
\item{TST.FRAC}{[0.1] set this fraction of data aside for testing (only for TST.kind=="rand")} 
\item{TST.SEED}{[NULL] a seed for the random test set selection. If NULL, use \code{\link{tdmRandomSeed}}. } 
\item{CLS.cutoff}{[NULL] vote fractions for the n.class classes. The class i with maximum ratio (\% votes)/CLS.cutoff[i] wins. 
If NULL, then each class gets the cutoff 1/n.class (i.e. majority vote wins) }
\item{CLS.CLASSWT}{[NULL] class weights for the n.class classes, e.g. c(10,20) for n.class=2. The higher, the more costly
is a misclassification of that real class). NULL for equal weights for each class.} 
\item{CLS.gainmat}{[NULL] (n.class x n.class) gain matrix. If NULL, CLS.gainmat will be set to unit matrix in \code{\link{tdmClassify}} }
\item{PRE.PCA}{["none" (default)|"linear"] PCA preprocessing: [don't | normal pca (prcomp) ] } 
\item{PRE.npc}{[0] if >0: add monomials of degree 2 for the first PRE.npc columns (PCs)} 
\item{SRF.kind}{["xperc" (default) |"ndrop" |"nkeep" |"none" ] the method used for feature selection, see \code{\link{tdmModSortedRFimport}}  } 
\item{SRF.ndrop}{   [0] how many variables to drop (if SRF.kind=="ndrop")  }
\item{SRF.XPerc}{  [0.95] if >=0, keep that importance percentage, starting with the most important variables (if SRF.kind=="xperc")  }
\item{SRF.calc}{   [T] =T: calculate importance & save on SRF.file, =F: load from SRF.file
(SRF.file = Output/<filename>.SRF.<response.variable>.Rdata) }
\item{SRF.ntree}{  [50] number of RF trees }
\item{SRF.samp}{    sampsize for RF }
\item{SRF.verbose}{ [2] }
\item{SRF.maxS}{    [40] how many variables to show in plot }
\item{SRF.minlsi}{  [1] a lower bound for the length of SRF$input.variables  }
\item{MOD.SEED}{[NULL] a seed for the random model initialization (if model is non-deterministic). If NULL, use \code{\link{tdmRandomSeed}}. } 
\item{MOD.method}{["RF" (default) |"MC.RF" |"SVM" |"NB" ]: use [RF | MetaCost-RF | SVM | Naive Bayes ] in \code{\link{tdmClassify}}  \cr
["RF" (default) |"SVM" |"LM" ]: use [RF | SVM | linear model ] in \code{\link{tdmRegress}}  } 
\item{RF.ntree}{[500] } 
\item{RF.samp}{[1000] } 
\item{RF.mtry}{[NULL] } 
\item{RF.nodesize}{[1] } 
\item{RF.OOB}{[T] if =T, return OOB-training set error as tuning measure; if =F, return test set error } 
\item{SVM.gamma}{[0.005] } 
\item{SVM.epsilon}{[0.005] needed only for regression} 
\item{SVM.cost}{[1.0] } 
\item{SVM.C}{[1] needed only for regression} 
\item{SVM.tolerance}{[0.008] } 
\item{rgain.type}{["rgain" (default) |"meanCA" |"minCA" ] in case of \code{\link{tdmClassify}}: For classification, the measure 
returned from \code{\link{tdmClassifyLoop}} in \code{result$R_*} is
[relative gain (i.e. gain/gainmax) | mean class accuracy | minimum class accuracy ]. The goal is to maximize  \code{Rgain}. \cr
For regression, the goal is to minimize \code{result$R_*} returned from \code{\link{tdmRegress}}. In this case, possible values are 
\code{rgain.type} = ["rmae" (default) |"rmse" ] which stands for [ relative mean absolute error | root mean squared error ].  } 
\item{ncopies}{[0] if >0, activate \code{\link{tdmParaBootstrap}} in \code{\link{tdmClassify}}  } 
\item{DO.POSTPROC}{[F] =T: call the user-defined postprocessing fct opts$fct.postproc after model building and its application to the test set.} 
\item{fct.postproc}{[NULL] a function with signature \code{(pred, dframe, opts)} where \code{pred} is the prediction of the model on the 
data frame \code{dframe} and \code{opts} is this list. This function may do some postprocessing on \code{pred}  and
it returns a (potentially modified) \code{pred}. This function will be called in \code{\link{tdmClassify}} if \code{DO.POSTPROC=T}.  }
\item{GD.DEVICE}{["win"] ="win": all graphics to (several) windows (\code{windows} or \code{X11} in package \code{grDevices}) \cr
="pdf": all graphics to one multi-page PDF \cr
="png": all graphics in separate PNG files in \code{opts$GD.PNGDIR} \cr
="non": no graphics at all \cr
This concerns the TDMR graphics, not the SPOT (or other tuner) graphics    } 
\item{GD.RESTART}{[T] =T: restart the graphics device (i.e. close all 'old' windows or re-open 
multi-page pdf) in each call to \code{\link{tdmClassify}} or \code{\link{tdmRegress}}, resp. \cr
=F: leave all windows open (suitable for calls from SPOT) or write more pages in same pdf. } 
\item{GD.CLOSE}{[T] =T: close graphics device "png", "pdf" at the end of main_*.r (suitable for main_*.r solo) or \cr
=F: do not close (suitable for call from tdmStartSpot, where all windows should remain open)  } 
\item{NRUN}{[2] how many runs with different train & test samples  - or - how many CV-runs, if \code{opts$TST.kind}="cv"  } 
\item{VERBOSE}{[2] =2: print much output, =1: less, =0: none}}
\seealso{\code{\link{tdmOptsDefaultsFill}}}
\author{Wolfgang Konen, FHK, Mar'2011 - Dec'2011}

