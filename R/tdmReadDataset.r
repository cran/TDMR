######################################################################################
# tdmReadDataset:
#     (simplified version of tdmReadData: two reading functions instead of opts$READ.CMD,
#      no variable argument 'filename' anymore, no save on .Rdata )
#
#' Read data accoroding to \code{opts} settings.
#'
#' Read the data accoring to the settings \code{opts$READ.*} and \code{opts$TST.COL} 
#' (see Details).
#'
#Details
#' When \code{opts$READ.TstFn==NULL}, then only \code{opts$READ.TrnFn} is used. \cr 
#' When \code{opts$READ.TstFn!=NULL}, the following things happen in \code{\link{tdmReadDataset}}: 
#' Data are read from \code{opts$filename} and from \code{opts$filetest}. Both data sets are bound 
#' together, with a new column \code{opts$TST.COL} having '0' for the data from opts$filename and 
#' having '1' for the data from opts$filetest. The branch using opts$TST.COL is invoked either with 
#' umode="TST" in \code{\link{unbiasedRun}} or with opts$TST.kind="col" in \code{\link{tdmModCreateCVindex}}.
#'
#' @param opts  list of options, we need here
#'  \itemize{
#'    \item \code{READ.TrnFn}: [tdmReadTrain] function with argument \code{opts} for reading  
#'                             the training data and returning them in a data frame
#'    \item \code{READ.NROW}:  [-1] read only that many rows from opts$filename. -1 for 'read all rows'.
#'    \item \code{READ.TstFn}: [NULL] function with argument \code{opts} for reading  
#'                             the test data and returning them in a data frame. If NULL
#'                             then skip test file reading.
#'    \item \code{TST.COL}:    ["TST.COL"] string, create a column with the name of this string 
#'                             in \code{dset}, which has 0 for training and 1 for test data   
#'    \item \code{path}:       used by READ.TrnFn to locate file
#'    \item \code{dir.data}:   used by READ.TrnFn to locate file
#'  }
#' @return \code{dset},  a data frame with all data read
#' @seealso \code{\link{tdmReadAndSplit}}
#'
#' @export
tdmReadDataset <- function(opts) {
  
    if (is.null(opts$READ.TrnFn)) 
      stop("opts$READ.TrnFn is missing. Need a function(opts) for reading the train-validation data!")
    if (is.null(opts$TST.COL)) opts$TST.COL = "TST.COL";
    
    if (opts$READ.TXT) {
      #if (!is.null(path)) opts$dir.data = paste(path,opts$dir.data,sep="/");
      ## DON'T!!, READ.TrnFn and READ.TstFn should use opts$path and opts$dir.data
      
      cat1(opts,opts$filename,": Read data from",opts$filename,"...\n")
      dset <- opts$READ.TrnFn(opts);
      if (!is.null(opts$READ.TstFn)) {
        cat1(opts,opts$filename,": Read test data from",opts$filetest, "...\n");
        tset <- opts$READ.TstFn(opts);
      }
    } 
    if (!is.null(opts$READ.TstFn)) {
	    if (is.null(opts$TST.COL)) stop("Need a non-NULL definition for opts$TST.COL!");
      dset <- tdmBindResponse(dset,opts$TST.COL,rep(0,nrow(dset)))
      tset <- tdmBindResponse(tset,opts$TST.COL,rep(1,nrow(tset)))
      dset <- rbind(dset,tset)
    }
    cat1(opts,opts$filename,":", length(dset[,1]), "records read.\n")
    
    dset;
}

######################################################################################
# tdmReadTrain:
#
# Template function, a possible default for opts$READ.TrnFn.
#
tdmReadTrain <- function(opts) {
  dset = read.csv(file=paste(opts$path,opts$dir.txt, opts$filename,sep="/"), nrow=opts$READ.NROW);   # includes header=T, sep="," and dec="."
}

tdmReadTest <- function(opts) {
  dset = read.csv(file=paste0(opts$path,opts$dir.txt, opts$filetest, sep="/"), nrow=opts$READ.NROW);   # includes header=T, sep="," and dec="."
}