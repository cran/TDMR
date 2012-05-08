\name{tdmReadData}
\alias{tdmReadData}
\title{Read the data accoroding to the settings in \code{opts}.}
\usage{
  tdmReadData(opts)
}
\arguments{
  \item{opts}{list of options, we need here \itemize{ \item
  \code{filename}: string of filename in opts$dir.txt \item
  \code{filetest}: string of filename test data in
  opts$dir.txt (only if READ.TST=T) \item \code{READ.TXT}:
  =T: read from opts$filename, =F: load from .Rdata file
  \item \code{READ.CMD}:
  ["read.csv(file=paste(opts$dir.txt, filename, sep=\"\"),
  nrow=opts$READ.NROW)"] string with a file-read-command
  with placeholder 'filename' \item \code{READ.TST}: if =T:
  read also test data from \code{filetest} \item
  \code{TST.COL}: string, create a column with the name of
  this string in \code{dset}, which has 0 for training and
  1 for test data \item \code{READ.NROW}: [-1] read only
  that many rows from opts$filename. -1 for 'read all
  rows'.  }}
}
\value{
  \code{dset}, a data frame with all data read
}
\description{
  Side effect: data are saved on .Rdata file if
  opts$READ.TXT=TRUE (for quicker next-time access).  They
  are re-loaded from .Rdata if opts$READ.TXT=FALSE.
}
\details{
  When opts$READ.TST==T, the following things happen in
  \code{\link{tdmReadData}}: Data are read from
  opts$filename and from opts$filetest. Both data sets are
  bound together, with a new column opts$TST.COL having '0'
  for the data from opts$filename and having '1' for the
  data from opts$filetest.  The branch using opts$TST.COL
  is invoked either with umode="TST" in
  \code{\link{unbiasedRun}} or with opts$TST.kind="col" in
  \code{\link{tdmModCreateCVindex}}.
}

