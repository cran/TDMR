\name{tdmPreGroupLevels}
\alias{tdmPreGroupLevels}
\title{Group the levels of factor variable in dset[,colname]...}
\usage{tdmPreGroupLevels(dset, colname, opts)
}
\description{Group the levels of factor variable in \code{dset[,colname]}}
\details{This function reduces the number of levels for factor variables with too many levels.
It counts the cases in each level and orders them decreasingly. Binds the least
frequent levels together in a new level "OTHER" such that the remaining untouched
levels have more than opts$Xpgroup percent of all cases. OR binds the least
important levels together in "OTHER" such that the total number of new levels
is opts$MaxLevel, whichever number is smaller.
INPUT:}
\value{dset      data frame dset with dset[,colname] re-grouped}
\arguments{\item{dset}{data frame}
\item{colname}{name of column to be re-grouped}
\item{opts}{list, here we need \itemize{
\item  Xpgroup   [0.99]
\item  MaxLevel  [32]  (32 is the maximum number of levels allowed for RF)
}}
}
