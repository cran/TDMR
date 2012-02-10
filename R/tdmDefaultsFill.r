######################################################################################
# tdmDefaultsFill
#
#'   Default values for list \code{tdm}.  This list controls the tuning and unbiased evaluation phase.
#'
#'   When called with \code{tdm = tdmDefaultsFill()}, a new list \code{tdm} is created and returned.
#'   When called with \code{tdm = tdmDefaultsFill(mainFile="my.r",mainCommand="result<-my(opts)")}, a new list \code{tdm} is created
#'   and returned, with the elements mainFile and mainCommand set to the specified values.
#'   When called with \code{tdm = tdmDefaultsFill(tdm)}, an existing list \code{tdm} is filled with further default values.
#'   If tdm$mainFunc is missing, but tdm$mainFile exists, then tdmDefaultsFill will set tdm$mainFunc=sub(".r","",basename(tdm$mainFile),fixed=TRUE).
#'
#'   @param tdm         (optional)
#'   @param mainFile    (optional) if given, create or overwrite tdm$mainFile with this value
#'   @param mainCommand (optional) if given, create or overwrite tdm$mainCommand with this value
#'   @return \code{tdm}     the new / extended list,  where additional elements, if they are not yet def'd,  are set as: 
#'      \item{mainFunc}{\code{sub(".r","",basename(tdm$mainFile),fixed=TRUE)}, if tdm$mainFile is set, else \code{"mainFunc"}}
#'      \item{mainCommand}{\code{result <- tdm$mainFunc(opts)}}
#'      \item{unbiasedFunc}{["unbiasedRun"] which function to call for unbiased evaluation}
#'      \item{tuneMethod}{["spot"] other choices: "cmaes", "bfgs", ..., see \code{\link{tdmDispatchTuner}} }
#'      \item{nExperim}{[1]}
#'      \item{umode}{["RSUB"], one out of [ "RSUB" | "CV" | "TST" | "SP_T" ], see \code{\link{unbiasedRun}}}
#'      \item{timeMode}{[1] 1: proc time, 2: elapsed time}
#'      \item{fileMode}{[TRUE]}
#'      \item{experFile}{[NULL] where to append final results  }
#'      \item{theSpotPath}{[NA] use SPOT's package version}
#'      \item{parallelCPUs}{[1] 1: sequential, >1: parallel with snowFall and this many cpus}
#'      \item{startDir}{[getwd()]}
#'      \item{test2.string}{["default cutoff"] }
#'      \item{optsVerbosity}{[0] the verbosity for the unbiased runs}
#'      \item{nrun}{[5] number of runs for unbiased runs}
#'      \item{tstFrac}{[0.2] test set fraction for unbiased runs (only for umode="RSUB") }
#'      \item{tstCol}{["TST"] opts$TST.COL for unbiased runs (only for umode="TST") }
#'      \item{nfold}{[10] number of CV-folds for unbiased runs (only for umode="CV") }
#'
#' @author Wolfgang Konen, Patrick Koch, Oct'2011
#' @export
######################################################################################
tdmDefaultsFill <- function(tdm=NULL,mainFile=NULL,mainCommand=NULL) {
  if (is.null(tdm)) tdm <- list();

  if (!is.null(mainFile)) tdm$mainFile=mainFile;
  if (!is.null(mainCommand)) tdm$mainCommand=mainCommand;
  
  if (is.null(tdm$mainFunc))  {
    tdm$mainFunc <- ifelse(is.null(tdm$mainFile), "mainFunc", sub(".R","",sub(".r","",basename(tdm$mainFile),fixed=TRUE)));   
    # e.g. tdm$mainFunc="main_sonar" if tdm$mainFile="C:/myDir/main_sonar.r"
  }
  if (is.null(tdm$mainCommand)) tdm$mainCommand <- paste("result <- ", tdm$mainFunc,"(opts,dset=dset)",sep=" ");
  if (is.null(tdm$unbiasedFunc)) tdm$unbiasedFunc <- "unbiasedRun";
  if (is.null(tdm$tuneMethod)) tdm$tuneMethod <- "spot";
  if (is.null(tdm$nExperim)) tdm$nExperim <- 1;
  if (is.null(tdm$umode)) tdm$umode <- "RSUB";
  if (is.null(tdm$timeMode)) tdm$timeMode <- 1;   # user time
  if (is.null(tdm$fileMode)) tdm$fileMode <- TRUE;
  if (is.null(tdm$optsVerbosity)) tdm$optsVerbosity <- 0;   # the verbosity for the unbiased runs
  if (is.null(tdm$theSpotPath)) tdm$theSpotPath <- NA;
  if (is.null(tdm$parallelCPUs)) tdm$parallelCPUs <- 1;
  if (is.null(tdm$startDir)) tdm$startDir <- getwd();
  
  # code which was previously in unbiasedRun. Now we put it here and call tdmDefaultsFill from unbiasedRun
  # (cleaner code, less places where tdm values are set)
  if (is.null(tdm$test2.string)) tdm$test2.string="default cutoff";
  if (is.null(tdm$tstCol)) tdm$tstCol="TST";
  if (is.null(tdm$tstFrac)) tdm$tstFrac=0.2;
  if (is.null(tdm$nfold)) tdm$nfold=10;
  if (is.null(tdm$nrun)) tdm$nrun=5;

  tdm;
}
