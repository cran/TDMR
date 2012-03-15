\name{tdmSplitTestData}
\alias{tdmSplitTestData}
\title{Read and split the task data.}
\usage{tdmSplitTestData(opts, tdm, nExp=0)
}
\description{Read and split the task data.
Read the task data using \code{\link{tdmReadData}} and split them into a test part and 
a training/validation-part.}
\value{dataObj, either NULL (if opts$READ.INI==FALSE) or an object of class \code{TDMdata} containing
\item{dset}{ a data frame with the complete data set}
\item{TST.COL}{ string, the name of the column in \code{dset} which has a 1 for 
records belonging to the test set and a 0 for train/vali records. If tdm$umode=="SP_T", then 
TST.COL="tdmSplit", else TST.COL=opts$TST.COL. }
\item{filename}{ \code{opts$filename}, from where the data were read}

Known caller: \code{\link{tdmCompleteEval}}}
\seealso{\code{\link{dsetTrnVa.TDMdata}}, \code{\link{tdmReadData}}, \code{\link{tdmCompleteEval}}}
\author{Wolfgang Konen, FHK, Mar'2012}
\arguments{\item{opts}{a list from which we need here the elements
\itemize{
\item \code{READ.INI}:  [T] =T: do read and split, =F: return NULL   
\item \code{READ.*}:  other settings for \code{\link{tdmReadData}}  
\item \code{filename}:  needed for \code{\link{tdmReadData}}  
\item \code{filetest}:  needed for \code{\link{tdmReadData}}  
\item \code{TST.testFrac}:  [0.1] set this fraction of the daa aside for testing 
\item \code{TST.COL}:   string with name for the partitioning column, if tdm$umode is not "SP_T".
(If tdm$umode=="SP_T", then TST.COL="tdmSplit" is used.)
}}
\item{tdm}{a list from  which we need here the elements 
\itemize{
\item \code{mainFile}:  if not NULL, set working dir to \code{dir(mainFile)} before executing  \code{\link{tdmReadData}} 
\item \code{umode}:  [ "RSUB" | "CV" | "TST" | "SP_T" ], how to divide in training/validation data for tuning
and test data for the unbiased runs  
\item \code{SPLIT.SEED}:  if not NULL, set random number generator to this value --> deterministic test set split
\item \code{stratified}: [NULL] if not NULL, do the split by stratified sampling (at least one record of each class level
found in \code{dset[,tdm$stratified]} shall appear in the train-vali-set, recommended for classification)
}}
\item{nExp}{[0] experiment counter, used to select a reproducible different seed, if tdm$SPLIT.SEED!=NULL}
}

