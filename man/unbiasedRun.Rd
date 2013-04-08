\name{unbiasedRun}
\alias{unbiasedRun}
\title{Perform unbiased runs with best-solution parameters.}
\usage{
  unbiasedRun(confFile, envT, dataObj = NULL,
    finals = NULL, umode = "RSUB", withParams = FALSE,
    tdm = NULL)
}
\arguments{
  \item{confFile}{the .conf filename, e.g.
  "appAcid_02.conf"}

  \item{envT}{environment, from which we need the objects
  \describe{ \item{\code{bst}}{ data frame containing best
  results (merged over repeats)} \item{\code{res}}{ data
  frame containing all results} \item{\code{theTuner}}{
  ["spot"] string} \item{\code{spotConfig}}{ [NULL] a list
  with SPOT settings. If NULL, try to read spotConfig from
  confFile.} }}

  \item{dataObj}{[NULL] contains the pre-fetched data from
  which we use here the test-set part.  If NULL, set it to
  \code{tdmSplitTestData(opts,tdm)}}

  \item{finals}{[NULL] a one-row data frame to which new
  columns with final results are added}

  \item{umode}{[ "RSUB" (default) | "CV" | "TST" | "SP_T"
  ], how to divide in training and test data for the
  unbiased runs: \describe{ \item{\code{"RSUB"}}{ random
  subsampling into (1-tdm$TST.testFrac)\% training and
  tdm$TST.testFrac\% test data} \item{\code{"CV"}}{ cross
  validation (CV) with tdm$nrun folds} \item{\code{"TST"}}{
  all data in opts$filename are used for training, all data
  in opts$filetest for testing} \item{\code{"SP_T"}}{
  'split_test': prior to tuning, the data set was split by
  random subsampling into tdm$TST.testFrac\% test and
  (1-tdm$TST.testFrac)\% training-vali data, tagged via
  column "tdmSplit". Tuning was done on training-vali data.
  Now we use column "tdmSplit" to select the test data for
  unbiased evaluation. Training during unbiased evaluation
  is done on a fraction tdm$TST.trnFrac of the
  training-vali data} }}

  \item{withParams}{[FALSE] if =TRUE, add columns with best
  parameters to data frame finals (should be FALSE, if
  different runs have different parameters)}

  \item{tdm}{a list with TDM settings from which we use
  here the elements \describe{ \item{mainFunc}{ the
  function to be called for unbiased evaluations}
  \item{mainFile}{ change to the directory of mainFile
  before starting mainFunc} \item{nrun}{ [5] how often to
  call the unbiased evaluation} \item{nfold}{ [10] how many
  folds in CV (only relevant for umode="CV") }
  \item{TST.testFrac}{ [0.2] test set fraction (only
  relevant for umode="RSUB" or ="SP_T") } } The defaults in
  '[...]' are set by \code{\link{tdmDefaultsFill}}, if they
  are not defined on input.}
}
\value{
  \code{finals} a one-row data frame with final results
}
\description{
  Read the best solution of a parameter-tuning run from
  \code{envT$bst}, execute with these best parameters the
  function \code{tdm$mainFunc} (usually a classification or
  regression machine learning task), to see whether the
  result quality is reproducible on independent test data
  or on independently trained models.
}
\note{
  Side Effects: The list \code{result}, an object of class
  \code{\link{TDMclassifier}} or \code{\link{TDMregressor}}
  as returned from \code{tdm$mainFunc} is written onto
  \code{envT$result}. \cr If \code{envT$spotConfig} is
  NULL, it is constructed from confFile. \cr If
  \code{spotConfig$opts} (list with all parameter settings
  for the DM task) is NULL, we try to read it from
  \code{spotConfig$io.apdFileName}. This will issue a
  warning '... might not work in parallel mode', but is
  perfectly fine for non-parallel mode.
}
\examples{
## Load the best results obtained in a prior tuning for the configuration
   ## "sonar_04.conf" with tuning method "spot". The result envT from a prior
   ## run of tdmBigLoop with this .conf is read from demo02sonar/demoSonar.RData.
   ## Run task main_sonar again with these best parameters, using the default
   ## settings from tdmDefaultsFill: umode="RSUB", tdm$nrun=5  and tdm$TST.testFrac=0.2.
   oldwd <- getwd();
   ## The best results are read from demo02sonar/demoSonar.RData relative to
   ## the TDMR package directory.
   setwd(paste(find.package("TDMR"), "demo02sonar",sep="/"));
   load("demoSonar.RData");
   source("main_sonar.r");
   finals <- unbiasedRun("sonar_04.conf",envT,tdm=envT$tdm);
   print(finals);
   setwd(oldwd);
}
\author{
  Wolfgang Konen, FHK, 2010 - 2013
}
\seealso{
  \code{\link{tdmBigLoop}}, \code{\link{TDMclassifier}},
  \code{\link{TDMregressor}}
}

