\name{tdmOptsDefaultsSet}
\alias{tdmOptsDefaultsSet}
\title{Default values for list opts.}
\description{Default values for list \code{opts}. Set up and return a list \code{opts} with default settings.}
\details{For better readability, the elements of  \code{opts} are arranged in groups:
\tabular{ll}{
\code{dir.*} \tab  path-related settings  \cr
\code{READ.*} \tab  data-reading-related settings  \cr
\code{TST.*} \tab  resampling-related settings (training, validation and test set, CV)  \cr    
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

The path-related settings are relative to \code{dir(tdm$mainFile)}, if it is def'd, else relative to the current dir.}
\value{a list \code{opts}, with defaults set for all options relevant for a DM task, 
containing the following elements
\item{dir.txt}{[./data] where to find .txt/.csv files} 
\item{dir.data}{[./data] where to find data files} 
\item{dir.Rdata}{[./Rdata] where to find .Rdata files} 
\item{dir.output}{[./Output] where to put output files} 
\item{filename}{["default.txt"] the task data} 
\item{filetest}{[NULL] the test data, only relevant for READ.TST=T} 
\item{data.title}{["Default Data"] title for plots} 
\item{READ.TXT}{[T] =T: read data from .csv and save as .Rdata, =F: read from .Rdata}                                                   
\item{READ.NROW}{[-1] read this amount of rows or -1 for 'read all rows'} 
\item{READ.TST}{[F] =T: read unseen test data from opts$filetest (usually you will do this only for the final model and only with TST.kind="col")} 
\item{READ.CMD}{["read.csv(file=paste(opts$dir.txt, filename, sep=\"\"), nrow=opts$READ.NROW)"] 
the command to be passed into \code{\link{tdmReadData}}. It has to contain the placeholder 'filename'. The default 
in brackets implies the settings header=T, sep="," and dec="."   } 
\item{READ.INI}{[TRUE] read the task data initially, i.e. prior to tuning, using \code{\link{tdmReadData}} .  
If =FALSE, the data are read anew in each pass through main_TASK, i.e. in each tuning step (deprecated). } 
\item{TST.kind}{["rand"] one of the choices from \{"cv","rand","col"\}, see \code{\link{tdmModCreateCVindex}} for details  } 
\item{TST.COL}{[NULL] name of column with train/test/disregard-flag or NULL} 
\item{TST.NFOLD}{[3] number of CV-folds (only for TST.kind=="cv")} 
\item{TST.valiFrac}{[0.1] set this fraction of data aside for validation (only for TST.kind=="rand")} 
\item{TST.testFrac}{[0.1] set prior to tuning this fraction of data aside for testing (if tdm$umode=="SP_T" and opts$READ.INI==TRUE)
or set this fraction of data aside for testing after tuning (if tdm$umode=="RSUB" or =="CV") } 
\item{TST.SEED}{[NULL] a seed for the random test set selection (\code{\link{tdmRandomSeed}}) and random validation set selection. 
(\code{\link{tdmClassifyLoop}}). If NULL, use \code{\link{tdmRandomSeed}}. } 
\item{CLS.cutoff}{[NULL] vote fractions for the n.class classes. The class i with maximum ratio (\% votes)/CLS.cutoff[i] wins. 
If NULL, then each class gets the cutoff 1/n.class (i.e. majority vote wins) }
\item{CLS.CLASSWT}{[NULL] class weights for the n.class classes, e.g. c(10,20) for n.class=2. The higher, the more costly
is a misclassification of that real class). NULL for equal weights for each class.} 
\item{CLS.gainmat}{[NULL] (n.class x n.class) gain matrix. If NULL, CLS.gainmat will be set to unit matrix in \code{\link{tdmClassify}} }
\item{PRE.PCA}{["none" (default)|"linear"] PCA preprocessing: [don't | normal pca (prcomp) ] } 
\item{PRE.PCA.REPLACE}{[T] =T: replace with the PCA columns the original numerical columns, =F: add the PCA columns} 
\item{PRE.PCA.npc}{[0] if >0: add monomials of degree 2 for the first PRE.PCA.npc columns (PCs)} 
\item{PRE.SFA}{["none" (default)|"2nd"] SFA preprocessing (see package \code{\link{rSFA}}: [don't | ormal SFA with 2nd degree expansion ] } 
\item{PRE.SFA.REPLACE}{[F] =T: replace the original numerical columns with the SFA columns; =F: add the SFA columns } 
\item{PRE.SFA.npc}{[0] if >0: add monomials of degree 2 for the first PRE.SFA.npc columns } 
\item{PRE.SFA.PPRANGE}{[11] number of inputs after SFA preprocessing, only those inputs enter into SFA expansion } 
\item{PRE.SFA.ODIM}{[5] number of SFA output dimensions (slowest signals) to return } 
\item{PRE.SFA.doPB}{[T] =T/F: do / don't do parametric bootstrap for SFA in case of marginal training data } 
\item{PRE.SFA.fctPB}{[sfaPBootstrap] the function to call in case of parametric bootstrap, see \code{\link{sfaPBootstrap}} 
in package \code{\link{rSFA}} for its interface description } 
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
\item{RF.OOB}{[T] if =T, return OOB-training set error as tuning measure; if =F, return validation set error } 
\item{SVM.gamma}{[0.005] } 
\item{SVM.epsilon}{[0.005] needed only for regression} 
\item{SVM.cost}{[1.0] } 
\item{SVM.C}{[1] needed only for regression} 
\item{SVM.tolerance}{[0.008] } 
\item{CLS.cutoff}{ [NULL] vote fractions for the n.class classes. The class i with maximum ratio (% votes)/RF.cutoff[i] wins. 
If NULL, then each class gets the cutoff 1/n.class (i.e. majority vote wins).   }
\item{CLS.CLASSWT}{ [NULL] class weights for the n.class classes, e.g. \cr
c(10,20) for n.class=2         \cr
(the higher, the more costly is a misclassification of that real class). NULL for no weights.  }
\item{CLS.gainmat}{ [NULL]  if [NULL], opts$CLS.gainmat will be set to unit matrix in \code{\link{tdmClassify}}   }
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
\note{The variables opts$PRE.PCA.numericV and opts$PRE.SFA.numericV (string vectors of numeric input columns to be used for PCA or SFA) 
are not set by \code{\link{tdmOptsDefaultsSet}} or \code{\link{tdmOptsDefaultsFill}}. Either they are supplied by the user or, 
if NULL, TDMR will set them to \code{input.variables} in \code{\link{tdmClassifyLoop}}, assuming that all columns are numeric. 
If PCA is done, its output \code{pca$numeric.variables} will overwrite \code{opts$PRE.SFA.numericV} (because the numeric variables 
after PCA become the input for SFA).}
\seealso{\code{\link{tdmOptsDefaultsFill}}}
\author{Wolfgang Konen, FHK, Mar'2011 - Feb'2012}

