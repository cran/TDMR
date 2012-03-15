\name{tdmPlotResMeta}
\alias{tdmPlotResMeta}
\title{Interactive plots of RES data frames and their metamodels.}
\usage{tdmPlotResMeta(envT)
}
\description{Interactive plots of RES data frames and their metamodels.
Makes interactive plots for any of the result data frames contained in \code{envT} together with fitted metamodels.
\code{tdmPlotResMeta} creates a \code{\link{twiddle}} interface which allows to select
\itemize{
\item tuner:      one of the tuners
\item nExperim:   a knob to select one of the experiments in \code{envT} (only if  \code{envT$tdm$nExperim > 1})
\item reportFunc: one of \{"spotReport3d", "spotReportContour"\}, see \code{\link{SPOT}} 
\item modelFit:   one of \{"spotPredictGausspr", "spotPredictRandomForest", "spotPredictMlegp"\}, see \code{\link{SPOT}} 
}}
\note{Side Effects:
One or several RGL plot windows are created which can be manipulated interactively. A certain RGL window \code{n} can be selected with 
\code{rgl.set(n)}. An interactively manipulated RGL window can be saved with \code{rgl.snapshot("myplot.png")}.}
\seealso{\code{\link{tdmCompleteEval}}}
\author{Wolfgang Konen}
\arguments{\item{envT}{environment with results as returned from \code{\link{tdmCompleteEval}} or as loaded from an appropriate .RData file}
}
\examples{# Read previous tuning results 'envT' from demo02sonar/sonar_04.RData (relative to the TDMR package directory). 
# Then, \code{tdmPlotResMeta} lets you explore interactively the RES data frame(s):
\dontrun{ 
load(paste(.find.package("TDMR"), "demo02sonar","sonar_04.RData",sep="/"));
tdmPlotResMeta(envT);
}}

