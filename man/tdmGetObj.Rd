\name{tdmGetObj}
\alias{tdmGetObj}
\title{helper fct to get bst or res object...}
\usage{tdmGetObj(envObj, objFileName, theTuner, tdm)
}
\description{helper fct to get \code{bst} or \code{res} object}
\details{If envObj is not NULL, return envObj.
If envObj is NULL, try to read the corresponding file from \code{theTuner/objFileName} or, 
if dir \code{tuner} does not exist, from \code{objFileName} in current dir and return the data frame read. 
This function should only be called if tdm$fileMode==T, otherwise the files might 
be missing or contain old information.}
\value{envObj}
\seealso{\code{\link{unbiasedBestRun_C}}, \code{\link{tdmCompleteEval}}}
\arguments{\item{envObj}{object, either envT$bst or envT$res, or NULL}
\item{objFileName}{alternative file to get the result from (.bst or .res file)}
\item{theTuner}{alternative dir where to look for objFileName}
\item{tdm}{here only needed for tdm$optsVerbosity}
}

