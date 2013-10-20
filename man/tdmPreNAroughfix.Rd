\name{tdmPreNAroughfix}
\alias{tdmPreNAroughfix}
\title{Replace <NA> values with suitable non <NA> values}
\usage{
  tdmPreNAroughfix(object, ...)
}
\arguments{
  \item{object}{list or data frame}

  \item{...}{additional arguments}
}
\value{
  \code{object}, the list or data frame with <NA> values
  replaced
}
\description{
  This function replaces <NA> values in a list entry or
  data frame column with the median (for numeric columns)
  or the most frequent mode (for factor columns). It does
  the same as \code{na.roughfix} in package randomForest,
  but does so faster.
}

