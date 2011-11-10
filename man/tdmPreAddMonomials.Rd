\name{tdmPreAddMonomials}
\alias{tdmPreAddMonomials}
\title{Add monomials of degree 2 to a data frame...}
\usage{tdmPreAddMonomials(dset, rx, degree=2, prefix="R")
}
\description{Add monomials of degree 2 to a data frame}
\details{Given the data frame \code{dset} and a data frame \code{rx} with the same number of rows,
add monomials of degree 2 to dset for all quadratic combinations of the columns
in rx. The naming of these new columns is "R1x2" for the combination of cols
1 and 2 and so on (if prefix="R").}
\value{data frame \code{dset} with the new monomial columns appended}
\note{CAVEAT: The double for-loop costs some time (e.g. 2-4 sec for ncol(rx)=8 or 10)
How to fix: make a version w/o for-loop and w/o frequent assigns to dset (**TODO**)}
\arguments{\item{dset}{the target data frame}
\item{rx}{a data frame where to draw the monomials from}
\item{degree}{(currently only 2 is supported)}
\item{prefix}{character prefix for the monomial column names}
}

