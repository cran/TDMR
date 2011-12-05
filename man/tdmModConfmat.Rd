\name{tdmModConfmat}
\alias{tdmModConfmat}
\title{Calculate confusion matrix and gain (used by \link{tdmClassify})...}
\usage{tdmModConfmat(d, colreal, colpred, opts)
}
\description{Calculate confusion matrix and gain (used by \code{\link{tdmClassify}})}
\value{cm   a list containing: 
\item{mat}{ matrix with real class levels as rows, predicted class levels columns.
mat[R1,P2]: number of records with real class R1
predicted as class P2, if opts$rgain.type=="rgain".
If opts$rgain.type=="meanCA" or "minCA", then show this number as percentage
of "records with real class R1" (percentage of each row).
CAUTION: If colpred contains NA's, those cases are missing in mat (!)
(but the class errors are correct as long as there are no NA's in colreal)}
\item{cerr}{ class error rates, vector of size nlevels(colreal)+1.
cerr[X] is the misclassification rate for real class X.
cerr["Total"] is the total classification error rate.}
\item{gain}{ the total gain (sum of pointwise product gainmat*cm$mat)}
\item{gain.vector}{ gain.vector[X] is the gain attributed to real class label X.
gain.vector["Total"] is again the total gain.}
\item{gainmax}{    the maximum achievable gain, assuming perfect prediction}
\item{rgain}{      ratio gain/gainmax in percent, if opts$rgain.type=="rgain";
mean class accuracy percentage (i.e. mean(diag(mat)), if opts$rgain.type=="meanCA";
min class accuracy percentage (i.e. min(diag(mat)), if opts$rgain.type=="minCA";}}
\author{Wolfgang Konen, Patrick Koch \email{wolfgang.konen@fh-koeln.de}}
\arguments{\item{d}{data frame}
\item{colreal}{name of column in d which contains the real class}
\item{colpred}{name of column in d which contains the predicted class}
\item{opts,}{a list from which we use the elements: \itemize{
\item gainmat   the gain matrix for each possible outcome, same size as cm$mat (see below);
gainmat[R1,P2]: the gain associated with a record of real class R1 which we
predict as class P2. (gain matrix = - cost matrix)
\item rgain.type  {"rgain" | "meanCA" | "minCA" } see below, affects output cm$mat and cm$rgain
}}
}

