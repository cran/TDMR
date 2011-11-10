\name{tdmCompleteEval}
\alias{tdmCompleteEval}
\title{Tuning and unbiased evaluation in a big loop.}
\usage{tdmCompleteEval(runList, spotList, spotStep="auto", tdm)
}
\description{Tuning and unbiased evaluation in a big loop.
For each \code{.conf} file in \code{runList} do tuning (SPOT, CMA-ES or other) and one or several 
runs of \code{tdm$unbiasedFunc} (as many as \code{tdm$umode} has elements).}
\details{Tuning is skipped if the \code{.conf} file does not appear in \code{spotList} or if \code{!(spotStep=="auto")}. In this
case it is assumed then that the appropriate \code{.bst} and \code{.res} files exist already.}
\value{environment envT, containing
\item{res}{ data frame with results from last tuning (one line for each call of \code{tdmStart*})} 
\item{bst}{ data frame with the best-so-far results from last tuning (one line collected after each (SPO) step)}
\item{theFinals}{ data frame with one-line result for each triple \code{confFile,nExp,tuner}:
\preformatted{confFile tuner nExp [params] RGain.bst RGain.CV sdG.CV RGain.TST sdG.TST }
where each 'sdG.' denotes the standard deviation of the preceeding RGain
and where \code{[params]} is written depending on \code{tdm$withParams}.
\code{RGain} denotes the relative gain on a certain data set: the actual gain
achieved with the model divided by the maximum gain possible for the 
current cost matrix and the current data set.}
\item{result}{ list with results of \code{tdm$mainCommand} as called in the last unbiased evaluation}}
\note{Side Effects:
If \code{tdm$fileMode==TRUE}, several files are written in the directory of the \code{.conf} file:
\itemize{
\item \code{envT } is saved to \code{<runList[1]>.rda} file
\item \code{envT$theFinals } is written to file \code{tdm$finalFile} and appended to \code{tdm$experFile}
\item \code{envT$res } is written to \code{.res} file in directory \code{<tuneMethod>}
\item \code{envT$bst } is written to \code{.bst} file in directory \code{<tuneMethod>}
}
More precisely: If we make tuning with \code{tuneMethod="lhd"} and perform the 3rd experiment for \code{.conf} file
\code{cpu_01.conf}, then the \code{.res} file is written to \code{lhd/cpu_01_lhd_03.res} relative to the directory 
of \code{.conf} file. Analoguously for \code{.bst} file.

An example usage of function tdmCompleteEval is shown in demo/demo01cpu.r, accessible via \code{demo(demo01cpu)}}
\author{Wolfgang Konen, Patrick Koch}
\arguments{\item{runList}{vector of \code{.conf} filenames}
\item{spotList}{\code{[NULL]} vector of \code{.conf} filenames for which spot tuning is done.
If \code{NULL}, then \code{spotList=runList}.}
\item{spotStep}{\code{["auto"]} which step of SPOT to execute (either \code{"auto"} or \code{"rep"}). Entries in this vector are
cyclically recycled if \code{spotStep} is shorter than \code{runList}.}
\item{tdm}{a list from which we need here the elements
\describe{
\item{\code{mainCommand}}{ the command to be called for unbiased evaluations}
\item{\code{mainFile}}{ change to the directory of mainFile before starting mainCommand}
\item{\code{tuneMethod}}{vector of tuning method(s) \code{"spot", "cmaes", "lhd", "bfgs", "powell"}}
\item{\code{unbiasedFunc}}{the function for unbiased evaluations to call}
\item{\code{umode}}{a vector of strings containing the unbiased resampling strategies
to execute \code{"RSUB", "TST", "CV"}, see \code{mapOpts} in tdmMapDesign.}
\item{\code{finalFile}}{filename where to save \code{envT$theFinals}}
\item{\code{withParams}}{\code{[NULL]} If \code{=TRUE}: include best parameters as columns in output \code{envT$theFinals}.  
If \code{=FALSE}: don't (this is appropriate if \code{runList} combines several .conf files which differ in their parameters)}
}}
}

