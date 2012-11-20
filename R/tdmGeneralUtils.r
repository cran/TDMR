######################################################################################
######################################################################################
# Package Description for Roxygene:
#' Tuned Data Mining in R
#'
#' \tabular{ll}{
#' Package: \tab TDMR\cr
#' Type: \tab Package\cr
#' Version: \tab 0.3.1\cr
#' Date: \tab 01.06.2012\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' TDMR is a package for tuned data mining (predictive analytics, i.e. \bold{classification} and \bold{regression}). Its main features are: \cr
#' 1) A variety of tuners, with special emphasis on \link{SPOT} (another well-known R package for parameter tuning), but also CMA-ES and other tuning algorithms. \cr
#' 2) Tuning of preprocessing (feature generation) parameters and model building parameters simultaneously.  \cr
#' 3) Support for multiple tuning experiments (different settings, repetitions with different resamplings, ...).  \cr
#' 4) Easy parallelization of those experiments with the help of R package \code{\link{snowfall}}.
#' 
#' The main entry point functions are \code{\link{tdmClassifyLoop}}, \code{\link{tdmRegressLoop}} and \code{\link{tdmBigLoop}}. 
#' See \code{\link{tdmOptsDefaultsSet}} and \code{\link{tdmDefaultsFill}} for an overview of adjustable TDMR-parameters.
#'
#' @name TDMR-package
#' @aliases TDMR
#' @docType package
#' @title Tuned Data Mining in R
#' @author Wolfgang Konen (\email{wolfgang.konen@@fh-koeln.de}), Patrick Koch
#' @references \url{http://gociop.de/research-projects/tuned-data-mining/}
#' @keywords package tuning data mining machine learning
#End of Package Description
NA #NULL, ends description without hiding first function
######################################################################################
######################################################################################



######################################################################################
#
# GENERAL UTILITY FUNCTIONS
#
######################################################################################
######################################################################################

######################################################################################
#' Bind a column to a data frame. 
#'
#' Bind the column with name \code{response.predict} and 
#' contents \code{vec} as last column to data frame \code{d}
#' @param d data frame
#' @param response.predict name of new column 
#' @param vec the contents for the last column bound to data frame \code{d}
#' @return data frame \code{d} with column added
#' @export
######################################################################################
tdmBindResponse <- function(d,response.predict,vec)
{
    if (is.na(match(response.predict,names(d)))) {
      # bind column response.predict as last column to data frame d
      eval(parse(text=paste("d <- cbind(d, ",response.predict,"=vec)")))
    } else {
      # replace contents of existing column response.predict
      d[,response.predict] <- vec;
    }

    return(d)
}
#
# this older version does the same, but requires three copy-replacements of
# data frame d (instead of one):
tdmBindResponse_OLD <- function(d,response.predict,vec)
{
    # drop column response.predict if there, do nothing if not there
    d <- d[,setdiff(names(d),response.predict)]
    # bind column response.predict as last column to data frame d
    d <- cbind(d, prediction=vec)
    names(d)[ncol(d)] <- response.predict

    return(d)
}

######################################################################################
# printout functions for different verbosity levels
######################################################################################
#' Output objects to \code{cat} if \code{opts$VERBOSE>=1}.
#'
#' @param opts from which we need the element VERBOSE
#' @param ...  objects
#' @return None
#' @seealso   \code{\link{cat}}
#' @export
#' @keywords internal
cat1 <- function(opts, ...) {  if (opts$VERBOSE>=1) cat(...); }
######################################################################################
#' Output objects to \code{cat} if \code{opts$VERBOSE>=2}.
#'
#' @param opts from which we need the element VERBOSE
#' @param ...  objects
#' @return None
#' @seealso   \code{\link{cat}}
#' @export
#' @keywords internal
cat2 <- function(opts, ...) {  if (opts$VERBOSE>=2) cat(...); }

######################################################################################
#' Print objects using \code{print} if \code{opts$VERBOSE>=1}.
#'
#' @param opts from which we need the element VERBOSE
#' @param ...  objects
#' @return None
#' @seealso   \code{\link{print}}
#' @export
#' @keywords internal
print1 <- function(opts, ...) {  if (opts$VERBOSE>=1) print(...); }

######################################################################################
#' Print objects using \code{print} if \code{opts$VERBOSE>=2}.
#'
#' @param opts from which we need the element VERBOSE
#' @param ...  objects
#' @return None
#' @seealso   \code{\link{print}}
#' @export
#' @keywords internal
print2 <- function(opts, ...) {  if (opts$VERBOSE>=2) print(...); }


######################################################################################
#
# UTILITY FUNCTIONS FOR TDMclassifier AND TDMregressor OBJECTS
#
######################################################################################
######################################################################################

predict.TDMclassifier <- function(result,newdata,...) {
  predict(result$lastRes$lastModel,newdata,...);
}

predict.TDMregressor <- function(result,newdata,...) {
  predict(result$lastRes$lastModel,newdata,...);
}

#summary.TDMclassifier <- function(result,...) {
#  cat("This is the TDMclassifier summary\n");
#}


RGainTST <- function(result) {
  if (!inherits(result, "TDMclassifier"))
        stop("This function is permitted only for objects of class `TDMclassifier'");
  mean(result$R_test);
}

RGainTRN <- function(result) {
  if (!inherits(result, "TDMclassifier"))
        stop("This function is permitted only for objects of class `TDMclassifier'");
  mean(result$R_train);
}

SRF <- function(result) {
  if (!inherits(result, "TDMclassifier"))
        stop("This function is permitted only for objects of class `TDMclassifier'");
  result$lastRes$SRF;
}

######################################################################################
#' Return the list 'opts'.
#'
#' Returns the list \code{opts} from objects of class \code{\link{TDMclassifier}}, 
#'                  \code{\link{TDMregressor}}, \code{\link[=tdmClassify]{tdmClass}} or \code{\link[=tdmRegress]{tdmRegre}}.
#'   @param x  an object of class \code{\link{TDMclassifier}}, \code{\link[=tdmClassify]{tdmClass}}, 
#'                                \code{\link{TDMregressor}} or \code{\link[=tdmRegress]{tdmRegre}}.
#'   @param ... -- currently not used -- 
#' @export
Opts <- function(x, ...)  UseMethod("Opts");

######################################################################################
#' @rdname Opts
#' @method Opts TDMclassifier
#' @export
# @keywords internal
Opts.TDMclassifier  <- function(x, ...) x$lastRes$opts;
#' @rdname Opts
#' @method Opts TDMregressor
#' @export
# @keywords internal
Opts.TDMregressor  <- function(x, ...) x$lastRes$opts;
#' @rdname Opts
#' @method Opts tdmClass
#' @export
# @keywords internal
Opts.tdmClass <- function(x,...) x$opts;
#' @rdname Opts
#' @method Opts tdmRegre
#' @export
# @keywords internal
Opts.tdmRegre <- function(x,...) x$opts;
#' @rdname Opts
#' @method Opts default
#' @export
# @keywords internal
Opts.default <- function(x, ...)  cat("This is Opts.default\n");
