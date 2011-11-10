\name{tdmModVote2Target}
\alias{tdmModVote2Target}
\title{Analyze how the vote fraction corresponds to reliability of prediction...}
\usage{tdmModVote2Target(vote0, pred, target)
}
\description{Analyze how the vote fraction corresponds to reliability of prediction
(for RF-prediction in case of binary (0/1) classification)}
\details{This function analyzes whether in different vote bins the trained RF makes
predictions with different reliability. Expected result: The larger the fraction
of trees voting for class 0 is, the smaller is the percentage of true class-1-
cases in this vote bin.
This function is somewhat specialized for the DMC2010-task.}
\value{a data frame with columns 
\item{vcut}{  vote cut v}
\item{count}{ number of cases with vote fraction in [v[i-1],v[i]]}
\item{pred0}{ fraction of 0-predictions}
\item{pCorr}{ fraction of correct predictions}
\item{pR}{    fraction of true 1-cases}}
\author{Wolfgang Konen \email{wolfgang.konen@fh-koeln.de}}
\arguments{\item{vote0}{vector: which fraction of trees votes for class 0?}
\item{pred}{vector: the predicted class for each record (0/1)}
\item{target}{vector: the true class for each vector (0/1)}
}

