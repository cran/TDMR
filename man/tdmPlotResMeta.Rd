\name{tdmPlotResMeta}
\alias{tdmPlotResMeta}
\title{Interactive plots of RES data frames and their metamodels.}
\usage{
  tdmPlotResMeta(envT)
}
\arguments{
  \item{envT}{environment with results as returned from
  \code{\link{tdmBigLoop}} or as loaded from an appropriate
  .RData file.  We use here especially \code{envT$resGrid},
  \code{envT$tdm} and \code{envT$tunerVal} (the
  \code{spotConfig} from the last tuning experiment).}
}
\description{
  Makes interactive plots for any of the result data frames
  contained in \code{envT} together with fitted metamodels.
  \code{tdmPlotResMeta} creates a
  \code{\link[twiddler]{twiddle}} interface which allows to
  select \itemize{ \item tuner: one of the tuners \item
  nExperim: a knob to select one of the experiments in
  \code{envT} (only if \code{envT$tdm$nExperim > 1}) \item
  reportFunc: one of \{"spotReport3d",
  "spotReportContour"\}, see \code{\link{SPOT}} \item
  modelFit: one of \{"spotPredictGausspr",
  "spotPredictRandomForest", "spotPredictForrester",
  "spotPredictMlegp"\}, see \code{\link{SPOT}} \item
  skipLastSteps: [0] skip the skipLastSteps sequential
  steps from the selected RES data frame.  \item
  skipIncomplete: [FALSE] if this checkbox is checked (i.e.
  skipIncomplete==TRUE), then all CONFIGs having *less*
  than seq.design.maxRepeats repeats are skipped \item
  nSkip: [0] skip the nSkip worst CONFIGs \item y10Exp: [0]
  multiply in the RES data frame the column Y (objective
  function) by the factor 10^y10Exp. This affects the
  coloring of the metamodel in case "spotReport3d": A
  surface in the Y-range [1e-3, 1e-2] would have only a
  single color, but a surface in the range [10,100] will
  have a richer color scheme.  }
}
\details{
  In case of "spotReport3d", the plot shows the metamodel
  as colored surface and the true observations of the
  objective function as black points. Each point stands for
  a CONFIG, i.e. it is an aggregation (usually: mean) of
  all repeats for a design point configuration. The options
  \code{nskip} and \code{skipIncomplete} allow to suppress
  certain CONFIGs which may be outliers and thus hinder the
  view on the 'interesting' region. Note however, that this
  suppression also affects the metamodel fit, so use these
  options with care.

  In case of "spotReportContour", the plot shows a contour
  plot of the metamodel with a black point indicating the
  best CONFIG found.

  If \code{envT$tunerVal$meta.compare} is TRUE, the quality
  of the metamodel is evaluated with the following
  procedure: A RES data frame \code{newRes} with N new
  design CONFIGs (not used during metamodel fit) is taken
  and their true target value (evaluation of target
  function) is compared with the metamodel's value. The
  mean absolute deviation \cr sum( | metamodel_i -
  newCONFIG_i | ) /N \cr is printed and returned in
  \code{envT$tunerVal$meta.newMAD}.  \cr The RES data frame
  \code{newRes} is either taken from
  \code{envT$tunerVal$meta.newRes} (if this element is not
  NULL) or it is taken from another member of envT$resGrid
  (preferably from a 'lhd' tuner result, because it is
  evenly distributed). \cr If
  \code{envT$tunerVal$meta.compare} is NULL, it is set to
  FALSE.
}
\note{
  Side Effects: In case of "spotReport3d", one or several
  RGL plot windows are created which can be manipulated
  interactively.  A certain RGL window \code{n} can be
  selected with \code{rgl.set(n)}.  An interactively
  manipulated RGL window can be saved with
  \code{rgl.snapshot("myplot.png")}.

  In case of "spotPredictMlegp" there is a new element
  sC$seq.mlegp.min.nugget=1 set in the source code which
  causes the MLEGP fit to become much smoother in the
  presence of a noisy target function (nugget effect).
}
\examples{
\dontrun{
     ##
     ## Read previous tuning results 'envT' from demo02sonar/demoSonar.RData
     ## (relative to the TDMR package directory).
     ## Then, tdmPlotResMeta lets you explore interactively the RES data frame(s):
     load(paste(find.package("TDMR"), "demo02sonar","demoSonar.RData",sep="/"));
     tdmPlotResMeta(envT);
   }
}
\author{
  Wolfgang Konen
}
\seealso{
  \code{\link{tdmBigLoop}}
}

