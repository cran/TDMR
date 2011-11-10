\name{tdmPrePCA}
\alias{tdmPrePCA}
\title{PCA (Principal Component Analysis) for numeric columns in a data frame...}
\usage{tdmPrePCA(dset, numeric.variables, opts)
}
\description{PCA (Principal Component Analysis) for numeric columns in a data frame}
\details{tdmPrePCA is capable of linear PCA, based on prcomp (which uses SVD), and 
of kernel PCA (either KPCA, KHA or KFA).}
\value{pca     a list with entries: \itemize{
\item    dset: the input data frame dset with columns numeric.variables replaced
by the PCs with names PC1, PC2, ... (in case PRE.PCA=="linear")
or with names KP1, KP2, ... (in case PRE.PCA=="kernel")
and optional with monomial columns added, if PRE.npc>0
\item    numeric.variables:   the new column names for PCs and for the monomials
\item    eigval:   the eigenvalues for the PCs
\item    time:     the processing time
}}
\note{CAUTION: Kernel PCA (opts$PRE.PCA=="kernel") is currently only experimental, it *crashes*
for large number of records or large number of columns.}
\arguments{\item{dset}{the data frame with training (and test) data.}
\item{numeric.variables}{vector with all column names in dset for which PCA is performed.
These columns may contain *numeric* values only (at least for linear PCA).}
\item{opts}{a list from which we need here the following entries: \itemize{
\item    PRE.PCA:   ["linear" | "kernel" | "none" ]
\item    PRE.knum:  if >0 and if PRE.PCA="kernel", take only a subset of PRE.knum records from dset 
\item    PRE.npc:   if >0, then add for the first PRE.npc PCs the monomials of
degree 2 (see tdmPreAddMonomials)
\item    READ.TST:   if =T, then use only training records (those with dset[,opts$TST.COL]==0) during 
PC calculation, but perform the rotation to PC-space for all records.
}}
}

