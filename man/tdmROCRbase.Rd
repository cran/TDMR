% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tdmROCR.r
\name{tdmROCRbase}
\alias{tdmROCRbase}
\title{Single plot of ROC, lift or other chart for a \code{\link{TDMclassifier}} object.}
\usage{
tdmROCRbase(
  x,
  dataset = "validation",
  nRun = 1,
  typ = "ROC",
  noPlot = FALSE,
  ...
)
}
\arguments{
\item{x}{return value from a prior call to \code{\link{tdmClassifyLoop}}, an object of class \code{\link{TDMclassifier}}.}

\item{dataset}{["validation"] which part of the data to use, either "training" or "validation"}

\item{nRun}{[1] if x contains multiple runs, which run to show  (1,...,\code{\link{Opts}}(x)$NRUN)}

\item{typ}{["ROC"] which chart type, one out of ("ROC","lift","precRec") for 
             (ROC, lift, precision-recall)-chart (see \code{\link[ROCR]{performance}} in package ROCR for more details):
\itemize{
  \item "ROC":      receiver operating curve, TPR vs. FPR, with TPR=TP/(TP+FN)=TP/P and FPR=FP/(FP+TN)=FP/N (true and false positive rate).
  \item "lift":     lift chart, LIFT vs. RPP, with LIFT=TPR/RPR with random positive rate RPR=P/(P+N) and RPP=(TP+FP)/(P+N) (rate of pos. predictions).
  \item "precRec":  precision-recall-chart, PREC vs. RECALL, with PREC=TP/(TP+FP) and RECALL=TP/P (same as TPR).
 }}

\item{noPlot}{[FALSE] if TRUE, suppress the plot, return only the area under curve}

\item{...}{currently not used}
}
\value{
The area between the curve and the bottom line y=0.0 in the case of \code{typ=="ROC" | typ=="precRec"} \cr
     or  the area between the curve and the bottom line y=1.0 in the case of \code{typ=="lift"}. \cr
     If object \code{x} does not contain a prediction score, a warning is issued and the return value is NULL.
}
\description{
Single plot of ROC, lift or other chart for a \code{\link{TDMclassifier}} object.
}
\examples{
#*# --------- demo/demo05ROCR.r ---------
#*# Run task SONAR with "area under ROC curve" as performance measure (rgain.type="arROC").
#*# Other settings are similar to demo01-1sonar.r (level 1 of TDMR).
#*# Finally, plot ROC curve for validataion data set and 
#+#          plot lift chart for training data set
#*#
    path <- paste(find.package("TDMR"), "demo02sonar",sep="/");
    #path <- paste("../inst", "demo02sonar",sep="/");

    source(paste(path,"main_sonar.r",sep="/"));    # defines readTrnSonar

    controlDM <- function() {
      #
      # settings for the DM process (former sonar_00.apd file): 
      # (see ?tdmOptsDefaultsSet for a complete list of all default settings 
      # and many explanatory comments)
      #
      opts = list(path = path,
                  dir.data = "data",            # relative to path
                  filename = "sonar.txt",
                  READ.TrnFn = readTrnSonar,    # defined in main_sonar.r
                  data.title = "Sonar Data",
                  NRUN =  1,
                  rgain.type = "arROC",
                  VERBOSE = 2
      );
      opts <- setParams(opts, defaultOpts(), keepNotMatching = TRUE);
    }
    
    opts <- controlDM();
    result <- main_sonar(opts);     
    
    tdmGraphicNewWin(opts);
    cat("Area under ROC-curve for validation data set: ",
        tdmROCRbase(result),"\n");                                # side effect: plot ROC-curve
    tdmGraphicNewWin(opts);
    cat("Area under lift curve for  training data set: ",
        tdmROCRbase(result,dataset="training",typ="lift"),"\n");  # side effect: plot lift chart
    
}
\seealso{
\code{\link{tdmClassifyLoop}}   \code{\link{tdmROCR.TDMclassifier}}
}
