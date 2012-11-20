######################################################################################
# tdmReadData:
#
#'   Read the data accoroding to the settings in \code{opts}.
#'
#'   Side effect: Data are saved on .Rdata file if opts$READ.TXT=TRUE (for quicker next-time access).
#'	 Next-time access: Data are re-loaded from .Rdata if opts$READ.TXT=FALSE.
#'
#'   When opts$READ.TST==T, the following things happen in \code{\link{tdmReadData}}: 
#'   Data are read from opts$filename and from opts$filetest. Both data sets are bound together, with a new 
#'   column opts$TST.COL having '0' for the data from opts$filename and having '1' for the data from opts$filetest.
#'   The branch using opts$TST.COL is invoked either with umode="TST" in \code{\link{unbiasedRun}} or with 
#'   opts$TST.kind="col" in \code{\link{tdmModCreateCVindex}}.
#'
#' @note 
#'    If opts$READ.TXT=T and opts$READ.NROW>0, then only the first READ.NROW records are read from opts$filename and from opts$filetest.
#'    Only the records read are saved to the .Rdata files. This means: when starting tdmReadData again with opts$READ.TXT=F, 
#'    not more than these READ.NROW records can be retrieved from the relevant .Rdata files. To have again the full data sets on the
#'    .Rdata files, you need to invoke tdmReadData at least once with opts$READ.TXT=T and opts$READ.NROW==-1.    \cr
#'    If, on the other hand, the full data sets are stored in the .Rdata files, then opts$READ.TXT=F and opts$READ.NROW=<any value> 
#'    allows to select a smaller subset of records from the .Rdata files. 
#' 
#' @param opts  list of options, we need here
#'  \itemize{
#'    \item \code{filename}:   string of filename in opts$dir.txt 
#'    \item \code{filetest}:   string of filename test data in opts$dir.txt (only if READ.TST=T)
#'    \item \code{READ.TXT}:   =T: read from opts$filename, =F: load from .Rdata file 
#'    \item \code{READ.CMD}:   ["read.csv(file=paste(opts$dir.txt, filename, sep=\"\"), nrow=opts$READ.NROW)"] 
#'                             string with a file-read-command with placeholder 'filename' 
#'    \item \code{READ.TST}:   if =T: read also test data from \code{filetest} 
#'    \item \code{TST.COL}:    string, create a column with the name of this string in \code{dset}, which has 0 for training and 1 for test data 
#'    \item \code{READ.NROW}:  [-1] read only that many rows from opts$filename. -1 for 'read all rows'.
#'  }
#' @return \code{dset},  a data frame with all data read
#'
#' @export
tdmReadData <- function(opts) {
    if (is.null(opts$READ.CMD))
        opts$READ.CMD = "read.csv(file=paste(opts$dir.txt, filename, sep=\"\"), nrow=opts$READ.NROW)";   # includes header=T, sep="," and dec="."
    if (is.null(opts$filesuffix))
        stop("Cannot continue with opts$filesuffix==NULL. Please use 'opts=tdmOptsDefaultsSet()' to construct opts correctly.");
    suffix = opts$filesuffix;
    filename <- opts$filename;
    
    if (opts$READ.TXT) {
      cat1(opts,filename,": Read data from",filename,"...\n")
      cmd <- paste("dset <-",opts$READ.CMD);
      eval(parse(text=paste("dset <-",opts$READ.CMD)));
      #dset <- read.csv2(file=paste(opts$dir.txt, filename, sep=""), dec=".", sep=";", nrow=opts$READ.NROW,header=T)
      filenameRdata =  sub(suffix,".Rdata",filename);
      if (filenameRdata==filename)        # this should normally not happen. Just for safety, to avoid overwriting of filename
          stop(sprintf("filenameRdata=%s has to differ from filename=%s. Please check opts$filesuffix.",filenameRdata,filename)); 
      pathfile=paste(opts$dir.data,filenameRdata,sep="")
      if (!file.exists(dirname(pathfile))) {
        # why dirname(pathfile) and not simply opts$dir.data? - Because opts$dir.data has trailing "/" which confuses file.exists
        success = dir.create(opts$dir.data);     
        if (!success) stop(sprintf("Could not create opts$dir.data=%s",opts$dir.data));
      }
      save(dset, file=pathfile)
      if (opts$READ.TST) {
        cat1(opts,filename,": Read test data from",opts$filetest, "...\n");
        filename=opts$filetest;
        cmd <- paste("tset <-",opts$READ.CMD);
        eval(parse(text=paste("tset <-",opts$READ.CMD)));
        #tset <- read.csv2(file=paste(opts$dir.txt, opts$filetest, sep=""), dec=".", sep=";",header=T)  # read the test data
        filenameRdata =  sub(suffix,".Rdata",opts$filetest);
        if (filenameRdata==filename)      # this should normally not happen. Just for safety, to avoid overwriting of opts$filetest
            stop(sprintf("filenameRdata=%s has to differ from filetest=%s. Please check opts$filesuffix.",filenameRdata,opts$filetest)); 
        save(tset, file=paste(opts$dir.data,filenameRdata,sep=""))
      }
    } else {
      cat1(opts,filename,": Load data from .Rdata ...\n")
      load(file=paste(opts$dir.data,sub(suffix,".Rdata",opts$filename),sep=""))
      if (opts$READ.NROW>0)  dset <- dset[1:min(opts$READ.NROW,length(dset[,1])) , ];
      if (opts$READ.TST)
        load(file=paste(opts$dir.data,sub(suffix,".Rdata",opts$filetest),sep=""))
        if (opts$READ.NROW>0)  tset <- tset[1:min(opts$READ.NROW,length(dset[,1])) , ];
    }
    if (opts$READ.TST) {
	    if (is.null(opts$TST.COL)) stop("Need a non-NULL definition for opts$TST.COL!");
      dset <- tdmBindResponse(dset,opts$TST.COL,rep(0,nrow(dset)))
      tset <- tdmBindResponse(tset,opts$TST.COL,rep(1,nrow(tset)))
      dset <- rbind(dset,tset)
    }
    cat1(opts,opts$filename,":", length(dset[,1]), "records read.\n")

    dset;
}
