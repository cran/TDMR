\name{tdmPreAddMonomials}
\alias{tdmPreAddMonomials}
\title{Add monomials of degree 2 to a data frame.}
\usage{
  tdmPreAddMonomials(dset, rx, PRE.npc, opts, degree = 2,
    prefix = "R")
}
\arguments{
  \item{dset}{the target data frame}

  \item{rx}{a data frame where to draw the monomials from}

  \item{PRE.npc}{the number of columns from \code{rx} to
  use (clipped to \code{ncol(rx)} if necessary)}

  \item{opts}{a list from which we need here the following
  entries: \itemize{ \item filename \item VERBOSE }}

  \item{degree}{[2] (currently only 2 is supported)}

  \item{prefix}{["R"] character prefix for the monomial
  column names}
}
\value{
  data frame \code{dset} with the new monomial columns
  appended. If PRE.npc==0, the data frame is returned
  unchanged.
}
\description{
  Given the data frame \code{dset} and a data frame
  \code{rx} with the same number of rows, add monomials of
  degree 2 to dset for all quadratic combinations of the
  first PRE.npc columns of \code{rx}. The naming of these
  new columns is "R1x2" for the combination of cols 1 and 2
  and so on (if prefix="R").
}
\note{
  CAVEAT: The double for-loop costs some time (e.g. 2-4 sec
  for ncol(rx)=8 or 10) How to fix: make a version w/o
  for-loop and w/o frequent assigns to dset (**TODO**)
}

