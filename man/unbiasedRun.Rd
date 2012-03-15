\name{unbiasedRun}
\alias{unbiasedRun}
\title{Perform unbiased runs with best-solution parameters.}
\usage{unbiasedRun(confFile, envT, dataObj, finals, umode="RSUB", withParams=F, tdm)
}
\description{Perform unbiased runs with best-solution parameters.
Read the best solution of a parameter-tuning run (either from envT$bst or from file), 
run with these best parameters the command \code{tdm$mainCommand} (usually a classification or 
regression machine learning task), to see 
whether the result quality is reproducible on independent test data or on independently trained models.}
\details{If envT$bst or envT$res is NULL, try to read it from the file (the filename is
inferred envT$spotConfig. If this is NULL, it is constructed from confFile). 
We try to find the files for envT$bst or envT$res in dir envT$theTuner).}
\value{\code{finals}     a one-row data frame with final results}
\note{Side Effects:
The list \code{result} returned from \code{tdm$mainCommand} is written onto \code{envT$result}. \cr
If  \code{envT$spotConfig} is NULL, it is constructed from confFile. \cr
If \code{spotConfig$opts} (list with all parameter settings for the DM task) is NULL, we try to read it from  
\code{spotConfig$io.apdFileName}. This will issue a warning '... might not work in parallel mode', but is perfectly fine for non-parallel mode.}
\seealso{\code{\link{tdmCompleteEval}}}
\author{Wolfgang Konen, FHK, Sep'2010 - Dec'2011}
\arguments{\item{confFile}{the .conf filename, e.g. "appAcid_02.conf"}
\item{envT}{environment, from which we need the objects
\describe{
\item{\code{bst}}{ data frame containing best results (merged over repeats)}
\item{\code{res}}{ data frame containing all results}
\item{\code{theTuner}}{ ["spot"] string}
\item{\code{spotConfig}}{ [NULL] a list with SPOT settings. If NULL, try to read spotConfig from confFile.} 
}}
\item{dataObj}{[NULL] contains the pre-fetched data from which we use here the test-set part}
\item{finals}{[NULL] a one-row data frame to which new columns with final results are added}
\item{umode}{[ "RSUB" (default) | "CV" | "TST" | "SP_T" ], how to divide in training and test data for the unbiased runs:
\describe{
\item{\code{"RSUB"}}{ random subsampling into (1-tdm$tstFrac)\% training and tdm$tstFrac\% test data}
\item{\code{"CV"}}{ cross validation (CV) with tdm$nrun folds}
\item{\code{"TST"}}{ all data in opts$filename are used for training, all data in opts$filetest for testing}
\item{\code{"SP_T"}}{ 'split_test': prior to tuning, the data set was split by random subsampling into test and training-vali data. 
Tuning was done on training-vali data, now we use column "tdmSplit" to select the test data for unbiased evaluation.}
}}
\item{withParams}{[FALSE] if =TRUE, add columns with best parameters to data frame finals
(should be FALSE, if different runs have different parameters)}
\item{tdm}{a list with TDM settings from which we use here the elements
\describe{
\item{mainCommand}{ the command to be called for unbiased evaluations}
\item{mainFile}{ change to the directory of mainFile before starting mainCommand}
\item{nrun}{ [5] how often to call the unbiased evaluation}
\item{nfold}{ [10] how many folds in CV (only relevant for umode="CV") }
\item{tstFrac}{ [0.2] test set fraction (only relevant for umode="RSUB") }
}
The defaults in '[...]' are set by  \code{\link{tdmDefaultsFill}}, if they are not defined on input.}
}
\examples{# Load the best results obtained in a prior tuning for the configuration "sonar_04.conf" with tuning method "spot".
# Run task main_sonar again with these best parameters, using umode="RSUB" and from tdmDefaultsFill 
# the default settings tdm$nrun=5, tdm$tstFrac=0.2.
oldwd <- getwd();          
# The best results are read from demo02sonar/spot/sonar_04.bst relative to the TDMR package directory.
setwd(paste(.find.package("TDMR"), "demo02sonar",sep="/"));
envT <- new.env();
tdm <- list(mainFile="main_sonar.r", mainFunction="main_sonar", tuneMethod="spot");
if (!is.null(tdm$mainFile)) source(tdm$mainFile);
finals <- unbiasedRun("sonar_04.conf",envT,tdm=tdm)
print(finals);
setwd(oldwd);}

