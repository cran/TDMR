\name{cmaesTuner}
\alias{cmaesTuner}
\title{Perform CMA-ES tuning.}
\usage{cmaesTuner(confFile, tdm, envT, dataObj)
}
\description{Perform CMA-ES tuning. Perform a parameter tuning by CMA-ES, using the *R*-implementation 
(package \code{cmaes} by Olaf Mersmann).}
\value{result of CMA-ES algorithm}
\arguments{\item{confFile}{task configuration for tuning algorithm}
\item{tdm}{the TDMR object}
\item{envT}{the environment variable}
\item{dataObj}{the object with the data set (train/vali part and test part)}
}

