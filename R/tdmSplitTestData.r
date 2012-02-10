######################################################################################
# tdmSplitTestData
#
#'   Read and split the task data.
#'   Read the task data using \code{\link{tdmReadData}} and split them into a test part and 
#'   a training/validation-part.
#'
#'   @param opts a list from which we need here the elements
#'     \itemize{
#'       \item \code{READ.INI}:  [T] =T: do read and split, =F: return NULL   
#'       \item \code{READ.*}:  other settings for \code{\link{tdmReadData}}  
#'       \item \code{filename}:  needed for \code{\link{tdmReadData}}  
#'       \item \code{filetest}:  needed for \code{\link{tdmReadData}}  
#'       \item \code{TST.SEED}:  
#'       \item \code{TST.testFrac}:  [0.1] set this fraction of the daa aside for testing 
#'       \item \code{TST.COL}:   string with name for the partitioning column, if tdm$umode is not "SP_T".
#'                      (If tdm$umode=="SP_T", then TST.COL="tdmSplit" is used.)
#'     }
#'   @param tdm a list from  which we need here the elements 
#'     \itemize{
#'       \item \code{mainFile}:  if not NULL, set working dir to \code{dir(mainFile)} before executing  \code{\link{tdmReadData}} 
#'       \item \code{umode}:  [ "RSUB" | "CV" | "TST" | "SP_T" ], how to divide in training/validation data for tuning
#'                      and test data for the unbiased runs  
#'     }
#'   @return dataObj, either NULL (if opts$READ.INI==FALSE) or an object of class \code{TDMdata} containing
#'      \item{dset}{ a data frame with the complete data set}
#'      \item{TST.COL}{ string, the name of the column in \code{dset} which has a 1 for 
#'                      records belonging to the test set and a 0 for train/vali records. If tdm$umode=="SP_T", then 
#'                      TST.COL="tdmSplit", else TST.COL=opts$TST.COL. }
#'      \item{filename}{ \code{opts$filename}, from where the data were read}
#'
#'    Known caller: \code{\link{tdmCompleteEval}}
#'
#' @seealso   \code{\link{dsetTrnVa.TDMdata}}, \code{\link{tdmReadData}}
#' @author Wolfgang Konen, FHK, Feb'2012
#' @export
######################################################################################
tdmSplitTestData <- function(opts,tdm) {
	if (opts$READ.INI) {
		oldwd = getwd();                                               #
    if (!is.null(tdm$mainFile)) setwd(dirname(tdm$mainFile));      # save & change working dir
		dset <- tdmReadData(opts);
		setwd(oldwd);                                                  # restore working dir
		
		if (tdm$umode=="SP_T") {
    		L = nrow(dset);
        if (is.null(opts$TST.SEED)) {
          set.seed(tdmRandomSeed());
        } else {
          set.seed(opts$TST.SEED);
        }		
    		# simple random sampling: 
    		p <- sample(L);                  # random permutation of indices 1:L  
    		# calculate tfr, the record where the test set starts (TST.testFrac)
    		tfr <- (1-opts$TST.testFrac)*L;
    		cat1(opts,opts$filename,": Setting data aside for testing with opts$TST.testFrac = ",opts$TST.testFrac*100,"%\n");
    		cvi <- rep(0,L);
    		cvi[p[(tfr+1):L]] <- 1;          # test set index ( TST.testFrac  percent of the data)
    		TST.COL = "tdmSplit";
    		dset[,TST.COL] = cvi;			       # add a new column "tdmSplit" with 1 for test data, 0 else
    		
    		# TODO: add an option for stratified sampling (each level at least once in train-vali-set)
    		
		} else if (tdm$umode=="TST") {
    		TST.COL = opts$TST.COL;          # take the partition delivered by tdmReadData
		} else {  # i.e. if tdm$umode=="RSUB" or =="CV"
		    cvi = rep(0,nrow(dset));         # all data in dset used for training and validation
    		TST.COL = opts$TST.COL;          
        dset[,TST.COL] = cvi;			       # add a new column with only 0's
		}
		dataObj <- list(
			  dset=dset
			, TST.COL=TST.COL
			, filename=opts$filename
			);
		class(dataObj) <- "TDMdata";
	} else {   # i.e. if opts$READ.INI==FALSE
		dataObj <- NULL;
	}	
	
	dataObj;
}

dsetTrnVa <- function(x)  UseMethod("dsetTrnVa");
dsetTrnVa.default <- function(x)  stop("Method dsetTrnVa only allowed for objects of class TDMdata");
######################################################################################
# dsetTrnVa.TDMdata
#
#'   Return the train-validation part of a \code{TDMdata} object containing the task data.
#'
#'   @method dsetTrnVa TDMdata
#'   @param x  return value from a prior call to \code{\link{tdmSplitTestData}}, an object of class \code{TDMdata}.
#'
#' @seealso   \code{\link{tdmSplitTestData}}
#' @author Wolfgang Konen, FHK, Feb'2012
#' @export
######################################################################################
dsetTrnVa.TDMdata <- function(x)  {
  x$dset[x$dset[,x$TST.COL]==0,];	# return the training-validation part of data frame dset
}

# --- so far, method dsetTest is not needed by TDMR ---
dsetTest <- function(x, ...)  UseMethod("dsetTest");
dsetTest.default <- function(x, ...)  stop("Method dsetTest only allowed for objects of class TDMdata");
dsetTest.TDMdata <- function(x, ...)  {
	x$dset[x$dset[,x$TST.COL]==1,];	# return the test part of data frame dset
}
