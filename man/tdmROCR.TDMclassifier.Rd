\name{tdmROCR.TDMclassifier}
\alias{tdmROCR.TDMclassifier}
\title{Interactive plot of ROC, lift or other charts for a \code{\link{TDMclassifier}} object.}
\usage{
  \method{tdmROCR}{TDMclassifier} (x, ...)
}
\arguments{
  \item{x}{return value from a prior call to
  \code{\link{tdmClassifyLoop}}, an object of class
  \code{\link{TDMclassifier}}.}

  \item{...}{-- currently not used --}
}
\value{
  The area under the curve plotted most recently.
}
\description{
  Brings up a \code{\link{twiddle}} user interface, where
  the user may select a part of the dataset ("training" or
  "validation"), a run number (if
  \code{\link{Opts}}(x)$NRUN>1) and a type-of-chart, see
  \code{\link{tdmROCRbase}} for details. Using
  \code{\link{tdmROCRbase}}, the appropriate chart is
  plotted on the current graphics device.
}
\note{
  Side effect: Ror each chart, calculate and print the area
  between the curve and the bottom line (y=1.0 for
  \code{typ=="lift"}, y=0.0 else).
}
\examples{
\dontrun{
     setwd(paste(.find.package("TDMR"), "demo02sonar",sep="/"));
     source("main_sonar.r");
     result = main_sonar();
     tdmROCR(result);
   }
}
\seealso{
  \code{\link{tdmClassifyLoop}} \code{\link{tdmROCRbase}}
}
