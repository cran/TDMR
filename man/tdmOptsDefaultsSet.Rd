\name{tdmOptsDefaultsSet}
\alias{tdmOptsDefaultsSet}
\title{Default values for list opts.}
\description{Default values for list \code{opts}. Set up and return a list \code{opts} with default settings.}
\details{For all parameters that do NOT depend on previously def'd elements of \code{opts}.}
\value{a list \code{opts} with defaults set for all options relevant for a DM task, 
containing the following elements
\item{dir.data}{[./data] where to find data files} 
\item{dir.Rdata}{[./Rdata] where to find .Rdata files} 
\item{dir.txt}{[./data] where to find .txt/.csv files} 
\item{dir.output}{[./Output] where to put output files} 
\item{filename}{["default.txt"] the task data} 
\item{filesuffix}{[".txt"] suffix of filename} 
\item{data.title}{["Default Data"] title for plots} 
\item{READ.TXT}{[T] =T: read data from .csv and save as .Rdata, =F: read from .Rdata}                                                   
\item{READ.NROW}{[-1] read this amount of rows or -1 for 'read all rows' (only effective if READ.TXT=T)} 
\item{READ.TST}{[F] =T: read unseen test data (do this only for the final model and only with TST.kind="col")} 
\item{TST.kind}{[] .["cv"|"rand"|"col"] see tdmModCreateCVindex in tdmModelingUtils.r. Default is "rand"} 
\item{...}{[] ... to be continued ...} 
\item{...}{[] ... to be continued ...} 
\item{...}{[] ... to be continued ...}}

