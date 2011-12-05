\name{tdmOptsDefaultsFill}
\alias{tdmOptsDefaultsFill}
\title{Fill the current opts.}
\usage{tdmOptsDefaultsFill(opts, suffix)
}
\description{Fill the current \code{opts}. Fill the current \code{opts} with further default 
parameters if they are not yet defined. The defaults may depend on previously 
defined elements of \code{opts} (e.g. \code{opts$filename}).}
\value{\code{opts},  the extended options, where additional elements, if they are not yet def'd,  are set as: 
\item{filesuffix}{the suffix of \code{opts$filename}, e.g. \code{".csv"} } 
\item{TST.COL}{["TST.COL"] } 
\item{PDFFILE}{["*_pic.pdf"] file for multipage graphics in case \code{opts$GD.DEVICE}="pdf" } 
\item{GD.PNGDIR}{["PNG*"] directory for .png files in case \code{opts$GD.DEVICE}="png" } 
\item{LOGFILE}{["*.log"] where to log the output } 
\item{EVALFILE}{["*_eval.csv"] file with evaluation results allEVAL } 
\item{SRF.samp}{sample size for SRF, derived from \code{RF.samp} } 
\item{SRF.cutoff}{[CLS.cutoff] } 
\item{rgain.string}{ one out of c("RGain","MeanCA","MinCA","RMAE","RMSE"), depending on \code{opts$rgain.type} }

Here * is the stripped part of \code{opts$filename} (w/o suffix).

All files and directories in the above settings are relative to dir  \code{opts$dir.output}.}
\seealso{\code{\link{tdmOptsDefaultsSet}}}
\author{Wolfgang Konen, FHK, Mar'2011 - Dec'2011}
\arguments{\item{opts}{the options}
\item{suffix}{the suffix of \code{opts$filename}. If NULL, take opts$filesuffix (which, if also NUL, is inferred beforehand from opts$filename)}
}

