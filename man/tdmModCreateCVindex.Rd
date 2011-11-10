\name{tdmModCreateCVindex}
\alias{tdmModCreateCVindex}
\title{Create and return a training-test-set index vector...}
\usage{tdmModCreateCVindex(dset, response.variables, opts, stratified=FALSE)
}
\description{Create and return a training-test-set index vector}
\details{Depending on the value of TST.kind, the returned index cvi is
\enumerate{
\item TST.kind="cv": a random cross validation index P([111...222...333...]) - or -
\item TST.kind="rand": a random index with P([000...111...]) for training (0) and test (1) cases - or -
\item TST.kind="col": the column dset[,opts$TST.COL] contains the training (0) and test (1) set division
(and all records with a value <0 in column TST.COL are disregarded).
}
Here P(.) denotes random permutation of the sequence.
Special case TST.kind="cv" and TST.NFOLD=1: make *every* record a training record, i.e. index [000...]
In case TST.kind="rand" and stratified=TRUE a \emph{stratified} sample is drawn, where the strata in the 
training case reflect the rel. frequency of each level of the **1st** response variable
and are ensured to be at least of size 1.}
\value{cvi  training-test-set (0/>0) index vector
(all records with cvi<0, e.g. from column TST.COL, are disregarded)}
\note{Currently stratified sampling in case TST.KIND='rand' does only work correctly for \emph{one} response variable.  
If there are more than one, the right fraction of test records is taken, but the strata are drawn w.r.t. the 
first response variable. (For multiple response variables we would have to return a list of cvi's or to
call tdmModCreateCVindex for each response variable anew.)}
\arguments{\item{dset}{the data frame for which cvi is needed}
\item{response.variables}{issue a warning if \code{length(response.variables)>1}. Use the first
response variable for determining strata size.}
\item{opts}{a list from which we need here the following entries
\itemize{
\item TST.kind:  ["cv"|"rand"|"col"]
\item TST.NFOLD: number of CV folds (only relevant in case TST.kind=="cv")
\item TST.COL:   column of dset containing the (0/1/<0) index (only relevant in case TST.kind=="col")
or NULL if no such column exists
\item TST.FRAC:  fraction of records to set aside for testing (only relevant in case TST.kind=="rand")
}}
\item{stratified}{[F] do stratified sampling for TST.kind="rand" with at least one training 
record for each response variable level (classification)}
}

