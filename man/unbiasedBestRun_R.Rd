\name{unbiasedBestRun_R}
\alias{unbiasedBestRun_R}
\title{Perform unbiased runs with best-solution parameters (regression).}
\usage{unbiasedBestRun_R(confFile, envT, finals, umode="RSUB", withParams=F, tdm=tdm)
}
\description{Perform unbiased runs with best-solution parameters (regression).
Read the best solution of a parameter-tuning run (either from envT$bst or from file), 
run with these best parameters the command \code{tdm$mainCommand} (usually a regression machine learning task), to see 
whether the result quality is reproducible on independent test data or on independently trained models.}
\details{If envT$bst or envT$res is NULL, try to read it from the file (the filename is
inferred from confFile or envT$spotConfig and we try to find it in dir envT$theTuner).}
\value{\code{finals}     a one-row data frame with final results}
\note{Side Effects:
The list \code{result} returned from \code{tdm$mainCommand} is written onto \code{envT$result}.}
\references{\code{\link{unbiasedBestRun_C}}}
\author{Wolfgang Konen, FHK, Sep'2010 - Oct'2011}
\arguments{\item{confFile}{the .conf filename, e.g. "appAcid_02.conf"}
\item{envT}{environment, from which we need the objects
\describe{
\item{\code{bst}}{ data frame containing best results (merged over repeats)}
\item{\code{res}}{ data frame containing all results}
\item{\code{theTuner}}{ ["spot"] string}
\item{\code{opts}}{ list with all parameter settings for the DM task, i.e. as read in from  spotConfig$io.apdFileName}
\item{\code{spotConfig}}{ [NULL] a list with SPOT settings. If NULL, try to read spotConfig from confFile.} 
}}
\item{finals}{[NULL] a one-row data frame to which new columns with final results are added}
\item{umode}{[ "RSUB" | "CV" | "TST" ], how to divide in training and test data for the unbiased runs, default is "RSUB", 
see \code{tdmMapOpts} in tdmMapDesign.r for further details.}
\item{withParams}{[FALSE] if =TRUE, add columns with best parameters to data frame finals
(should be FALSE, if different runs have different parameters)}
\item{tdm}{a list with TDM settings from which we need here the elements
\describe{
\item{mainCommand}{ the command to be called for unbiased evaluations}
\item{mainFile}{ change to the directory of mainFile before starting mainCommand}
\item{nrun}{ how often to call the unbiased evaluation}
}}
}
\examples{oldwd <- getwd();          
setwd(paste(.find.package("TDMR"), "demo01cpu",sep="/"));
envT <- new.env();
tdm <- list(mainFile="main_cpu.r", mainCommand="result <- main_cpu(opts)");
source(tdm$mainFile);
finals <- unbiasedBestRun_R("cpu_01.conf",envT,tdm=tdm)
print(finals);
setwd(oldwd);}

