\name{tdmMapDesLoad}
\alias{tdmMapDesLoad}
\title{Load the mapping files.}
\usage{tdmMapDesLoad(envT, tdm)
}
\description{Load the mapping files. Load the map files \code{"tdmMapDesign.csv"} and optionally 
also \code{"userMapDesign.csv"} and store them in \code{envT$map} and \code{envT$mapUser},
resp. These maps are used by \code{\link{tdmMapDesApply}}.}
\details{\code{"tdmMapDesign.csv"} is searched in the TDMR library path \code{.find.package("TDMR")}.
(For the developer version: \code{<tdm$tdmPath>/inst}).
\code{"userMapDesign.csv"} is searched in the path \code{dirname(tdm$mainFile)}.}
\arguments{\item{envT}{environment}
\item{tdm}{list}
}

