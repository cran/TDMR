\name{tdmReadCmd}
\alias{tdmReadCmd}
\title{Template function, the default for opts$READ.CMD.}
\usage{
  tdmReadCmd(filename, opts)
}
\arguments{
  \item{filename}{this is either opts$filename or
  opts$filetest}

  \item{opts}{list of options, we need here \itemize{ \item
  \code{dir.txt}: string, the directory where to read
  \code{filename} \item \code{READ.NROW}: [-1] read only
  that many rows from opts$filename. -1 for 'read all
  rows'.  }}
}
\value{
  \code{dset}, a data frame with all data read
}
\description{
  This template function is used by
  \code{\link{tdmReadData}} for data reading.
}
\note{
  This default template function will issue the command \cr
  \code{dset=read.csv(file=paste(opts$dir.txt, filename,
  sep=""),nrow=opts$READ.NROW)} \cr which includes the
  default settings \code{header=T, sep="," and dec="."}.
  The user may alternatively specify his/her own function,
  the only rules are \itemize{ \item{ that it has to return
  a data frame (\code{dset} or \code{tset}) } \item{ that
  the string \code{opts$READ.CMD} contains an argument
  \code{filename} } } (\code{\link{tdmReadData}} will call
  this function with either \code{opts$filename} or
  \code{opts$filetest} replacing \code{filename})
}

