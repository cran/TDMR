\name{tdmGetObj}
\alias{tdmGetObj}
\title{Helper fct to get bst or res object from envT.}
\usage{tdmGetObj(envObj, objFileName, theTuner, tdm)
}
\description{Helper fct to get \code{bst} or \code{res} object from \code{envT}.}
\details{If \code{envObj} is not NULL, return envObj.
If \code{envObj} is NULL, try to read the corresponding file from \code{theTuner/objFileName} ( 
if dir \code{theTuner} does not exist, from \code{objFileName} in current dir) and return the data frame read. 
If \code{envObj} is NULL, this function should only be called when tdm$fileMode==T (or was =T in the prior tuning run), 
otherwise the files might be missing or contain old information.}
\value{envObj}
\seealso{\code{\link{unbiasedRun}}, \code{\link{tdmCompleteEval}}}
\arguments{\item{envObj}{object, either \code{envT$bst} or \code{envT$res}, or NULL}
\item{objFileName}{alternative file to get the result from (.bst or .res file)}
\item{theTuner}{alternative dir where to look for objFileName}
\item{tdm}{here only needed for tdm$optsVerbosity}
}

