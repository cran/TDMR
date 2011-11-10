\name{tdmPreLevel2Target}
\alias{tdmPreLevel2Target}
\title{Relate levels of a column with a target (column).}
\usage{tdmPreLevel2Target(dset, target, f, opts)
}
\description{Relate levels of a column with a target (column). Print for each level of factor variable f which ratio 0 / 1 of the binary target
variable it contains and how many cases are in each level}
\note{SIDE EFFECTS:
some printed output}
\arguments{\item{dset}{data frame}
\item{target}{name of target column}
\item{f}{number of column with factor variable}
\item{opts}{list, here we need \itemize{
\item opts$thresh_pR
\item opts$verbose
}}
}

