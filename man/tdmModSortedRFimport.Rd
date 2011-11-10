\name{tdmModSortedRFimport}
\alias{tdmModSortedRFimport}
\title{Return the list of input variables sorted decreasingly by their RF-importance...}
\usage{tdmModSortedRFimport(d_train, response.variable, input.variables, opts)
}
\description{Return the list of input variables sorted decreasingly by their RF-importance
(use na.roughfix for missing value replacement)
Decide which input variables to keep and return them in SRF$input.variables}
\value{SRF    a list with the following elements:
\item{input.variables}{   the vector of input variables which remain after importance
processing. These are sorted by decreasing importance.}
\item{s_input}{all input.variables sorted by decreasing (**NEW**) importance}
\item{s_imp1}{ the importance for s_input}
\item{s_dropped}{   vector with name of dropped variables}
\item{lsd}{    length of s_dropped}
\item{perc}{   the percentage of total importance which is in the dropped variables}
\item{opts}{   some defaults might have been added}}
\author{Wolfgang Konen, Patrick Koch \email{wolfgang.konen@fh-koeln.de}}
\arguments{\item{d_train}{training set}
\item{response.variable}{the target column from \code{d_train} to use for the RF-model}
\item{input.variables}{the input columns from \code{d_train} to use for the RF-model}
\item{opts}{options, here we use the elements [defaults in brackets]:
\itemize{
\item SRF.kind:  \itemize{
\item ="xperc": keep a certain importance percentage, starting from the most important variable
\item ="ndrop": drop a certain number of least important variables
\item ="nkeep": keep a certain number of most important variables
\item ="none": do not call tdmModSortedRFimport at all (see tdmRegress.r & tdmClassify.r)
}
\item SRF.ndrop:   [0] how many variables to drop (if SRF.kind=="ndrop")
\item SRF.XPerc:   [0.95] if >=0, keep that importance percentage, starting with the most
important variables (if SRF.kind=="xperc")
\item SRF.calc:    [TRUE] =TRUE: calculate importance & save on SRF.file, =F: load from SRF.file
(SRF.file = Output/<filename>.SRF.<response.variable>.Rdata)
\item SRF.ntree:   [50] number of RF trees
\item SRF.samp    sampsize for RF
\item SRF.verbose [2]
\item SRF.maxS    [40] how many variables to show in plot
\item SRF.minlsi  [1] a lower bound for the length of SRF$input.variables
\item GRAPHDEV  if !="non", then make a bar plot on current graphic device
\item CLASSWT     class weight vector to use in random forest training
}}
}

