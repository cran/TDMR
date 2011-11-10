######################################################################################
# tdmGetObj: 
#
#' helper fct to get \code{bst} or \code{res} object
#'   
#'   If envObj is not NULL, return envObj.
#'   If envObj is NULL, try to read the corresponding file from \code{theTuner/objFileName} or, 
#'   if dir \code{tuner} does not exist, from \code{objFileName} in current dir and return the data frame read. 
#'   This function should only be called if tdm$fileMode==T, otherwise the files might 
#'   be missing or contain old information.
#'
#'   @param envObj      object, either envT$bst or envT$res, or NULL
#'   @param objFileName alternative file to get the result from (.bst or .res file)
#'   @param theTuner    alternative dir where to look for objFileName
#'   @param tdm         here only needed for tdm$optsVerbosity
#'
#'   @return envObj
#'
#' @seealso \code{\link{unbiasedBestRun_C}}, \code{\link{tdmCompleteEval}}
#' @export
######################################################################################

tdmGetObj <- function(envObj,objFileName, theTuner,tdm) {
  if (is.null(envObj)) {
    tunedir = ifelse(file.exists(theTuner),paste(theTuner,"/",sep=""), "");
    # downward compatibility: if subdir envT$theTuner (e.g. "spot") is not present, take the current dir
    tBstFile = paste(tunedir,objFileName,sep="");
    if (!file.exists(tBstFile))
      stop(paste("Could not find file",tBstFile));
    suffix = unlist(strsplit(objFileName,".",fixed=T))[2]
  	if (tdm$optsVerbosity>0) writeLines(paste("Loading", suffix, "file data from:", tBstFile), con=stderr());
    obj <- read.table(tBstFile, sep=" ", header = TRUE);
  } else {
    obj <- envObj;
  }
  obj;
}

