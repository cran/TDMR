\name{tdmOptsDefaultsSet}
\alias{tdmOptsDefaultsSet}
\title{Default values for list \code{opts}.}
\usage{
  tdmOptsDefaultsSet(opts = NULL, path = "./")
}
\arguments{
  \item{opts}{(optional) the options already set}

  \item{path}{["./"] the path string to precede all
  settings \code{opts$dir.*}.  If \code{path=="./"} then
  nothing is preceeded to \code{opts$dir.*}.}
}
\value{
  a list \code{opts}, with defaults set for all options
  relevant for a DM task, containing the following elements
  \item{dir.txt}{[<path>/data] where to find .txt/.csv
  files} \item{dir.data}{[<path>/data] where to find other
  data files, including .Rdata }
  \item{dir.Rdata}{[<path>/Rdata] -- deprecated, use
  opts$dir.data -- } \item{dir.output}{[<path>/Output]
  where to put output files}
  \item{filename}{["default.txt"] the task data}
  \item{filetest}{[NULL] the test data, only relevant for
  READ.TST=T} \item{fileMode}{[TRUE] if =T, write
  opts$EVALFILE=*_train_eval.csv, *_train.csv.SRF.*.RData
  file and *_train.log file} \item{data.title}{["Default
  Data"] title for plots} \item{READ.TXT}{[T] =T: read data
  from .csv and save as .Rdata, =F: read from .Rdata}
  \item{READ.NROW}{[-1] read this amount of rows or -1 for
  'read all rows'} \item{READ.TST}{[F] =T: read unseen test
  data from opts$filetest (usually you will do this only
  for the final model and only with TST.kind="col")}
  \item{READ.CMD}{["\code{\link{tdmReadCmd}(filename,opts)}"]
  the command to be passed into \code{\link{tdmReadData}}.
  It has to contain the placeholder 'filename'. The default
  in brackets implies 'read.csv(file=paste(opts$dir.txt,
  filename, sep=""), nrow=opts$READ.NROW)' which includes
  the further settings header=T, sep="," and dec="."  }
  \item{READ.INI}{[TRUE] read the task data initially, i.e.
  prior to tuning, using \code{\link{tdmReadData}} .  If
  =FALSE, the data are read anew in each pass through
  main_TASK, i.e. in each tuning step (deprecated). }
  \item{TST.kind}{["rand"] one of the choices from
  \{"cv","rand","col"\}, see
  \code{\link{tdmModCreateCVindex}} for details }
  \item{TST.COL}{["TST.COL"] name of column with
  train/test/disregard-flag} \item{TST.NFOLD}{[3] number of
  CV-folds (only for TST.kind=="cv")}
  \item{TST.valiFrac}{[0.1] set this fraction of the
  train-validation data aside for validation (only for
  TST.kind=="rand")} \item{TST.testFrac}{[0.1] set prior to
  tuning this fraction of data aside for testing (if
  tdm$umode=="SP_T" and opts$READ.INI==TRUE) or set this
  fraction of data aside for testing after tuning (if
  tdm$umode=="RSUB" or =="CV") } \item{TST.trnFrac}{[NULL]
  train set fraction, if NULL then
  \code{\link{tdmModCreateCVindex}} will set it to 1 -
  opts$TST.valiFrac. } \item{TST.SEED}{[NULL] a seed for
  the random test set selection
  (\code{\link{tdmRandomSeed}}) and random validation set
  selection.  (\code{\link{tdmClassifyLoop}}). If NULL, use
  \code{\link{tdmRandomSeed}}. } \item{PRE.PCA}{["none"
  (default)|"linear"] PCA preprocessing: [don't | do normal
  PCA (prcomp) ] } \item{PRE.PCA.REPLACE}{[T] =T: replace
  with the PCA columns the original numerical columns, =F:
  add the PCA columns } \item{PRE.PCA.npc}{[0] if >0: add
  monomials of degree 2 from the first PRE.PCA.npc columns
  (PCs) (only active, if opts$PRE.PCA!="none")}
  \item{PRE.SFA}{["none" (default)|"2nd"] SFA preprocessing
  (see package \code{\link[rSFA]{rSFA}}: [don't | do ormal
  SFA with 2nd degree expansion ] }
  \item{PRE.SFA.REPLACE}{[F] =T: replace the original
  numerical columns with the SFA columns; =F: add the SFA
  columns } \item{PRE.SFA.npc}{[0] if >0: add monomials of
  degree 2 from the first PRE.SFA.npc columns (only acitve,
  if opts$PRE.SFA!="none") } \item{PRE.SFA.PPRANGE}{[11]
  number of inputs after SFA preprocessing, only those
  inputs enter into SFA expansion } \item{PRE.SFA.ODIM}{[5]
  number of SFA output dimensions (slowest signals) to
  return } \item{PRE.SFA.doPB}{[T] =F|T: don't | do
  parametric bootstrap for SFA in case of marginal training
  data } \item{PRE.SFA.fctPB}{[sfaPBootstrap] the function
  to call in case of parametric bootstrap, see
  \code{\link[rSFA]{sfaPBootstrap}} in package
  \code{\link[rSFA]{rSFA}} for its interface description }
  \item{PRE.allNonVali}{[F] if =T, then use all
  non-validation data in the training-validation set for
  PCA or SFA preprocessing.  If =F, use only the training
  set for PCA or SFA processing (only relevant if
  opts$PRE.PCA!="none" or opts$PRE.SFA!="none").  }
  \item{PRE.Xpgroup}{[0.99] bind the fraction 1-PRE.Xpgroup
  in column OTHER (see \code{\link{tdmPreGroupLevels}}) }
  \item{PRE.MaxLevel}{[32] bind the N-32+1 least frequent
  cases in column OTHER (see
  \code{\link{tdmPreGroupLevels}}) }
  \item{SRF.kind}{["xperc" (default) |"ndrop" |"nkeep"
  |"none" ] the method used for feature selection, see
  \code{\link{tdmModSortedRFimport}} } \item{SRF.ndrop}{
  [0] how many variables to drop (only relevant if
  SRF.kind=="ndrop") } \item{SRF.nkeep}{ [NULL] how many
  variables to keep, NULL="keep all" (only relevant if
  SRF.kind=="nkeep") } \item{SRF.XPerc}{ [0.95] if >=0,
  keep that importance percentage, starting with the most
  important variables (if SRF.kind=="xperc") }
  \item{SRF.calc}{ [T] =T: calculate importance & save on
  SRF.file, =F: load from srfFile (srfFile =
  Output/<confFile>.SRF.Rdata) } \item{SRF.ntree}{ [50]
  number of RF trees } \item{SRF.samp}{ sampsize for RF }
  \item{SRF.verbose}{ [2] } \item{SRF.maxS}{ [40] how many
  variables to show in plot } \item{SRF.minlsi}{ [1] a
  lower bound for the length of SRF$input.variables }
  \item{SRF.method}{ ["RFimp"] } \item{SRF.scale}{ [TRUE]
  option 'scale' for call importance() in
  \code{\link{tdmModSortedRFimport}} }
  \item{MOD.SEED}{[NULL] a seed for the random model
  initialization (if model is non-deterministic). If NULL,
  use \code{\link{tdmRandomSeed}}. }
  \item{MOD.method}{["RF" (default) |"MC.RF" |"SVM" |"NB"
  ]: use [RF | MetaCost-RF | SVM | Naive Bayes ] in
  \code{\link{tdmClassify}} \cr ["RF" (default) |"SVM"
  |"LM" ]: use [RF | SVM | linear model ] in
  \code{\link{tdmRegress}} } \item{RF.ntree}{[500] }
  \item{RF.samp}{[1000] } \item{RF.mtry}{[NULL] }
  \item{RF.nodesize}{[1] } \item{RF.OOB}{[TRUE] if =T,
  return OOB-training set error as tuning measure; if =F,
  return validation set error } \item{RF.p.all}{[FALSE] }
  \item{SVM.kernel}{[3] =1: linear, =2: polynomial, =3:
  RBF, =4: sigmoid} \item{SVM.epsilon}{[0.005] needed only
  for regression} \item{SVM.gamma}{[0.005] }
  \item{SVM.coef0}{[0.0] (needed only for
  opts$SVM.kernel=="polynomial" or =="sigmoid")}
  \item{SVM.degree}{[3] (needed only for
  opts$SVM.kernel=="polynomial")}
  \item{SVM.tolerance}{[0.008] } \item{ADA.coeflearn}{[1]
  =1: "Breiman", =2: "Freund", =3: "Zhu" as value for
  boosting(...,coeflearn,...) (AdaBoost) }
  \item{ADA.mfinal}{[10] number of trees in AdaBoost =
  mfinal boosting(...,mfinal,...)  }
  \item{ADA.rpart.minsplit}{[20] minimum number of
  observations in a node in order for a split to be
  attempted } \item{CLS.cutoff}{[NULL] vote fractions for
  the n.class classes. The class i with maximum ratio (\%
  votes)/CLS.cutoff[i] wins.  If NULL, then each class gets
  the cutoff 1/n.class (i.e. majority vote wins).  The
  smaller CLS.cutoff[i], the more likely class i will win.
  } \item{CLS.CLASSWT}{ [NULL] class weights for the
  n.class classes, e.g. \cr c(A=10,B=20) for a 2-class
  problem with classes A and B \cr (the higher, the more
  costly is a misclassification of that real class). It
  should be a named vector with the same length and names
  as the levels of the response variable. If no names are
  given, the levels of the response variables in
  lexicographical order will be attached in
  \code{\link{tdmClassify}}. CLS.CLASSWT=NULL for no
  weights.  } \item{CLS.gainmat}{[NULL] (n.class x n.class)
  gain matrix. If NULL, CLS.gainmat will be set to unit
  matrix in \code{\link{tdmClassify}} }
  \item{rgain.type}{["rgain" (default) |"meanCA" |"minCA" ]
  in case of \code{\link{tdmClassify}}: For classification,
  the measure \code{Rgain} returned from
  \code{\link{tdmClassifyLoop}} in \code{result$R_*} is
  [relative gain (i.e. gain/gainmax) | mean class accuracy
  | minimum class accuracy ].  The goal is to maximize
  \code{Rgain}. \cr For binary classification there are the
  additional measures [ "arROC" | "arLIFT" | "arPRE" ], see
  \code{\link{tdmModConfmat}}. \cr For regression, the goal
  is to minimize \code{result$R_*} returned from
  \code{\link{tdmRegress}}. In this case, possible values
  are \code{rgain.type} = ["rmae" (default) |"rmse" ] which
  stands for [ relative mean absolute error | root mean
  squared error ].  } \item{ncopies}{[0] if >0, activate
  \code{\link{tdmParaBootstrap}} in
  \code{\link{tdmClassify}} } \item{fct.postproc}{[NULL]
  name of a function with signature \code{(pred, dframe,
  opts)} where \code{pred} is the prediction of the model
  on the data frame \code{dframe} and \code{opts} is this
  list. This function may do some postprocessing on
  \code{pred} and it returns a (potentially modified)
  \code{pred}. This function will be called in
  \code{\link{tdmClassify}} if it is not \code{NULL}.  }
  \item{GD.DEVICE}{["win"] ="win": all graphics to
  (several) windows (\code{windows} or \code{X11} in
  package \code{grDevices}) \cr ="pdf": all graphics to one
  multi-page PDF \cr ="png": all graphics in separate PNG
  files in \code{opts$GD.PNGDIR} \cr ="non": no graphics at
  all \cr This concerns the TDMR graphics, not the SPOT (or
  other tuner) graphics } \item{GD.RESTART}{[T] =T: restart
  the graphics device (i.e. close all 'old' windows or
  re-open multi-page pdf) in each call to
  \code{\link{tdmClassify}} or \code{\link{tdmRegress}},
  resp. \cr =F: leave all windows open (suitable for calls
  from SPOT) or write more pages in same pdf. }
  \item{GD.CLOSE}{[T] =T: close graphics device "png",
  "pdf" at the end of main_*.r (suitable for main_*.r solo)
  or \cr =F: do not close (suitable for call from
  tdmStartSpot, where all windows should remain open) }
  \item{NRUN}{[2] how many runs with different train & test
  samples - or - how many CV-runs, if
  \code{opts$TST.kind}="cv" } \item{APPLY_TIME}{[FALSE] }
  \item{VERBOSE}{[2] =2: print much output, =1: less, =0:
  none}
}
\description{
  Set up and return a list \code{opts} with default
  settings. The list \code{opts} contains all DM-related
  settings which are needed by main_<TASK>.  \cr\cr For
  better readability, most elements of \code{opts} are
  arranged in groups: \tabular{ll}{ \code{dir.*} \tab
  path-related settings \cr \code{READ.*} \tab
  data-reading-related settings \cr \code{TST.*} \tab
  resampling-related settings (training, validation and
  test set, CV) \cr \code{PRE.*} \tab preprocessing
  parameters \cr \code{SRF.*} \tab several parameters for
  \code{\link{tdmModSortedRFimport}} \cr \code{MOD.*} \tab
  general settings for models and model building \cr
  \code{RF.*} \tab several parameters for model RF (Random
  Forest) \cr \code{SVM.*} \tab several parameters for
  model SVM (Support Vector Machines) \cr \code{ADA.*} \tab
  several parameters for model ADA (AdaBoost) \cr
  \code{CLS.*} \tab classification-related settings \cr
  \code{GD.*} \tab settings for the graphic devices \cr }
}
\details{
  The path-related settings are relative to
  \code{dir(tdm$mainFile)}, if it is def'd, else relative
  to the current dir. \cr Finally, the function
  \code{\link{tdmOptsDefaultsFill}(opts)} is called to fill
  in further details, depending on the current settings of
  \code{opts}.
}
\note{
  The variables opts$PRE.PCA.numericV and
  opts$PRE.SFA.numericV (string vectors of numeric input
  columns to be used for PCA or SFA) are not set by
  \code{\link{tdmOptsDefaultsSet}} or
  \code{\link{tdmOptsDefaultsFill}}. Either they are
  supplied by the user or, if NULL, TDMR will set them to
  \code{input.variables} in \code{\link{tdmClassifyLoop}},
  assuming that all columns are numeric.
}
\author{
  Wolfgang Konen, FHK, 2011 - 2013
}
\seealso{
  \code{\link{tdmOptsDefaultsFill}}
  \code{\link{tdmDefaultsFill}}
}

