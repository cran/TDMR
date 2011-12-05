######################################################################################
# tdmDefaultsFill
#
#'   Default values for list \code{tdm}.
#'
#'   When called with \code{tdm = tdmDefaultsFill()}, a new list \code{tdm} is created and returned.
#'   When called with \code{tdm = tdmDefaultsFill(mainFile="my.r",mainCommand="result<-my(opts)")}, a new list \code{tdm} is created
#'   and returned, with the elements mainFile and mainCommand set to the specified values.
#'   When called with \code{tdm = tdmDefaultsFill(tdm)}, an existing list \code{tdm} is filled with further default values.
#'
#'   @param tdm         (optional)
#'   @param mainFile    (optional) if given, create or overwrite tdm$mainFile with this value
#'   @param mainCommand (optional) if given, create or overwrite tdm$mainCommand with this value
#'   @return \code{tdm}     the new / extended list,  where additional elements, if they are not yet def'd,  are set as: 
#'      \item{tuneMethod}{[spot]}
#'      \item{nExperim}{[1]}
#'      \item{umode}{["DEF"], one out of [ "RSUB" | "CV" | "TST" | "DEF" ], see \code{\link{unbiasedRun}}}
#'      \item{timeMode}{[1] user time}
#'      \item{fileMode}{[TRUE]}
#'      \item{theSpotPath}{[NA] use SPOT's package version}
#'      \item{parallelCPUs}{[1]}
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
  
  if (is.null(tdm$tuneMethod)) tdm$tuneMethod <- "spot";
  if (is.null(tdm$nExperim)) tdm$nExperim <- 1;
  if (is.null(tdm$umode)) tdm$umode <- "DEF";
  if (is.null(tdm$timeMode)) tdm$timeMode <- 1;   # user time
  if (is.null(tdm$fileMode)) tdm$fileMode <- TRUE;
  if (is.null(tdm$optsVerbosity)) tdm$optsVerbosity <- 0;   # the verbosity for the unbiased runs
  if (is.null(tdm$theSpotPath)) tdm$theSpotPath <- NA;
  if (is.null(tdm$parallelCPUs)) tdm$parallelCPUs <- 1;
  if (is.null(tdm$startDir)) tdm$startDir <- getwd();
  
  if (is.null(tdm$test2.string)) tdm$test2.string="default cutoff";
  if (is.null(tdm$tstCol)) tdm$tstCol="TST";
  if (is.null(tdm$tstFrac)) tdm$tstFrac=0.2;
  if (is.null(tdm$nfold)) tdm$nfold=10;
  if (is.null(tdm$nrun)) tdm$nrun=5;

  tdm;
}
