\name{tdmPrePCA.train}
\alias{tdmPrePCA.train}
\title{PCA (Principal Component Analysis) for numeric columns in a data frame.}
\usage{tdmPrePCA.train(dset, opts)
}
\description{PCA (Principal Component Analysis) for numeric columns in a data frame.}
\details{tdmPrePCA.train is capable of linear PCA, based on prcomp (which uses SVD), and 
of kernel PCA (either KPCA, KHA or KFA).}
\value{\code{pca},     a list with entries: 
\item{dset}{  the input data frame dset with columns numeric.variables replaced or extended (depending on \code{opts$PRE.PCA.REPLACE})
by the PCs with names PC1, PC2, ... (in case PRE.PCA=="linear")
or with names KP1, KP2, ... (in case PRE.PCA=="kernel")
and optional with monomial columns added, if PRE.PCA.npc>0  }
\item{numeric.variables}{  the new numeric column names (PCs, monomials, and optionally old numericV, if \code{opts$PRE.PCA.REPLACE==F}) }
\item{pcaList}{  a list with the items \code{sdev, rotation, center, scale, x} as returned from \code{\link{prcomp}} 
plus \code{eigval}, the eigenvalues for the PCs }}
\note{CAUTION: Kernel PCA (opts$PRE.PCA=="kernel") is currently disabled in code, it *crashes*
for large number of records or large number of columns.}
\seealso{\code{\link{tdmPrePCA.apply}}}
\author{Wolfgang Konen, FHK, Mar'2011 - Jan'2012}
\arguments{\item{dset}{the data frame with training (and test) data.}
\item{opts}{a list from which we need here the following entries: \itemize{
\item    PRE.PCA:   ["linear" | "kernel" | "none" ]
\item    PRE.knum:  if >0 and if PRE.PCA="kernel", take only a subset of PRE.knum records from dset 
\item    PRE.PCA.REPLACE:  [T] =T: replace the original numerical columns with the PCA columns; =F: add the PCA columns
\item    PRE.PCA.npc:   if >0, then add for the first PRE.PCA.npc PCs the monomials of
degree 2 (see tdmPreAddMonomials)
\item    PRE.PCA.numericV   vector with all column names in dset for which PCA is performed.
These columns may contain *numeric* values only.
}}
}

