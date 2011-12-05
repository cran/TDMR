\name{tdmParaBootstrap}
\alias{tdmParaBootstrap}
\title{Parametric bootstrap: add 'noisy copies' to a data frame (training data).}
\usage{tdmParaBootstrap(dset, resp, input.variables, opts)
}
\description{Parametric bootstrap: add 'noisy copies' to a data frame (training data).}
\details{A normal distribution is approximated from the data given in \code{dset[,input.variables]} and for 
the columns \code{input.variables} new data are drawn from this distribution. The column \code{resp} 
is filled at random with levels with the same relative frequency as in \code{dset[,resp]}. 
Other columns of dset are filled by copying the entries from the first row of dset.}
\value{data frame \code{dset} with the new parametric bootstrap records added as last rows.}
\references{\code{\link{tdmClassify}}}
\author{Wolfgang Konen, FHK, Nov'2011-Dec'2011}
\arguments{\item{dset}{data frame with training set}
\item{resp}{name of column in dset which carries the target variable}
\item{input.variables}{vector with names of input columns}
\item{opts}{additional parameters [defaults in brackets]
\describe{
\item{\code{ncopies}}{ how many noisy copies to add }
\item{\code{ncsigma}}{ [1.0] multiplicative factor for each std.dev. }
\item{\code{ncmethod}}{ [3] which method to use for parametric bootstrap\cr
=1: each 'old' record from X in turn is the centroid for a new pattern;\cr
=2: the centroid is the average of all records from the same class, 
the std.dev. is the same for all classes;\cr
=3: centroid as in '2', the std.dev. is the std.dev. of
all records from the same class  (*recommended*)
}
\item{\code{TST.COL}}{ (optional) name of column in dset where each PB record is marked with a 0}
}}
}

