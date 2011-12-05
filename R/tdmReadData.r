######################################################################################
# tdmReadData:
#
#'    Read the data accoroding to the settings in \code{opts}.
#'
#'    Side effect: data are saved on .Rdata file or loaded from .Rdata (if opts$READ.TXT=F) for quicker access.
#'
#' @param opts  list of options, we need here
#'  \tabular{ll}{
#'    \code{filename} \tab  string of filename in opts$dir.data \cr
#'    \code{filetest} \tab  string of filename test data in opts$dir.data (only if READ.TST=T)\cr
#'    \code{READ.TXT} \tab  =T: read from opts$filename, =F: load from .Rdata file \cr
#'    \code{READ.CMD} \tab  string with a file-read-command with placeholder 'filename' \cr
#'    \code{READ.TST} \tab  if =T: read also data from \code{filetest} \cr
#'    \code{TST.COL}  \tab  create a column named TST.COL in \code{dset} which has 0 for training and 1 for test data \cr
#'    \code{READ.NROW}\tab  [-1] read only that many rows from opts$filename. -1 for 'read all rows'.
#'  }
#' @return \code{dset},  a data frame with all data read
#'
#' @export
tdmReadData <- function(opts) {
    if (is.null(opts$READ.CMD))
        opts$READ.CMD = "read.csv2(file=paste(opts$dir.txt, filename, sep=\"\"), dec=\".\", sep=\";\", nrow=opts$READ.NROW,header=T)";
    filename <- opts$filename;
    suffix = opts$filesuffix;
    
    if (opts$READ.TXT) {
      cat1(opts,filename,": Read data from",filename,"...\n")
      cmd <- paste("dset <-",opts$READ.CMD);
      eval(parse(text=paste("dset <-",opts$READ.CMD)));
      #dset <- read.csv2(file=paste(opts$dir.txt, filename, sep=""), dec=".", sep=";", nrow=opts$READ.NROW,header=T)
      save(dset, file=paste(opts$dir.data,sub(suffix,".Rdata",opts$filename),sep=""))
      if (opts$READ.TST) {
        cat1(opts,filename,": Read test data from",opts$filetest, "...\n");
        filename=opts$filetest;
        cmd <- paste("tset <-",opts$READ.CMD);
        eval(parse(text=paste("tset <-",opts$READ.CMD)));
        #tset <- read.csv2(file=paste(opts$dir.txt, opts$filetest, sep=""), dec=".", sep=";",header=T)  # read the test data
        save(tset, file=paste(opts$dir.data,sub(suffix,".Rdata",opts$filetest),sep=""))
      }
    } else {
      cat1(opts,filename,": Load data from .Rdata ...\n")
      load(file=paste(opts$dir.data,sub(suffix,".Rdata",opts$filename),sep=""))
      if (opts$READ.NROW>-1)  {
        if (opts$READ.NROW<length(dset[,1])) dset <- dset[1:opts$READ.NROW,];
      }
      if (opts$READ.TST)
        load(file=paste(opts$dir.data,sub(suffix,".Rdata",opts$filetest),sep=""))
    }
    if (opts$READ.TST) {
      dset <- bind_response(dset,opts$TST.COL,rep(0,nrow(dset)))
      tset <- bind_response(tset,opts$TST.COL,rep(1,nrow(tset)))
      dset <- rbind(dset,tset)
    }
    cat1(opts,opts$filename,":", length(dset[,1]), "records read.\n")

    dset;
}
