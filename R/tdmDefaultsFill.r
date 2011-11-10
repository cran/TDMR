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
#'   @return \code{tdm}        the new / extended list  with default values
#'      \item{tuneMethod}{[spot]}
#'      \item{nExperim}{[1]}
#'      \item{umode}{["DEF"], one out of [ "RSUB" | "CV" | "TST" | "DEF" ], see \code{\link{unbiasedBestRun_C}}}
#'      \item{timeMode}{[1] user time}
#'      \item{fileMode}{[TRUE]}
#'      \item{optsVerbosity}{[0] the verbosity for the unbiased runs}
#'      \item{theSpotPath}{[NA] use SPOT's package version}
#'      \item{parallelCPUs}{[1]}
#'      \item{startDir}{[getwd()]}
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
  
  tdm;
}
