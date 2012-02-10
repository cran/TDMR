\name{bfgsTuner}
\alias{bfgsTuner}
\title{Perform BFGS tuning.}
\usage{bfgsTuner(confFile, tdm, envT, dataObj)
}
\description{Perform BFGS tuning. Perform a parameter tuning by Broyden, Fletcher, Goldfarb and Shanno (BFGS) method.
The L-BFGS-B version allowing box constraints is used here.}
\value{result of BFGS algorithm}
\author{Wolfgang Konen, Patrick Koch \email{wolfgang.konen@fh-koeln.de}}
\arguments{\item{confFile}{task configuration for tuning algorithm}
\item{tdm}{the TDMR object}
\item{envT}{the environment variable}
\item{dataObj}{the object with the data set (train/vali part and test part)}
}

