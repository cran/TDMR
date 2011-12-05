\name{tdmReadData}
\alias{tdmReadData}
\title{Read the data accoroding to the settings in opts.}
\usage{tdmReadData(opts)
}
\description{Read the data accoroding to the settings in \code{opts}.}
\details{Side effect: data are saved on .Rdata file or loaded from .Rdata (if opts$READ.TXT=F) for quicker access.}
\value{\code{dset},  a data frame with all data read}
\arguments{\item{opts}{list of options, we need here
\tabular{ll}{
\code{filename} \tab  string of filename in opts$dir.data \cr
\code{filetest} \tab  string of filename test data in opts$dir.data (only if READ.TST=T)\cr
\code{READ.TXT} \tab  =T: read from opts$filename, =F: load from .Rdata file \cr
\code{READ.CMD} \tab  string with a file-read-command with placeholder 'filename' \cr
\code{READ.TST} \tab  if =T: read also data from \code{filetest} \cr
\code{TST.COL}  \tab  create a column named TST.COL in \code{dset} which has 0 for training and 1 for test data \cr
\code{READ.NROW}\tab  [-1] read only that many rows from opts$filename. -1 for 'read all rows'.
}}
}

