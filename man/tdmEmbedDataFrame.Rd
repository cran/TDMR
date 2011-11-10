\name{tdmEmbedDataFrame}
\alias{tdmEmbedDataFrame}
\title{Embed columns in a data frame...}
\usage{tdmEmbedDataFrame(x, cols, dimension=1, take=1)
}
\description{Embed columns in a data frame}
\details{Embeds the columns named \code{cols} in the data frame \code{x} into a space of dimension ##' \code{dimension}.}
\value{The data frame, augmented with embedded columns, shortended by \code{dimension} rows.}
\arguments{\item{x}{The data frame containing the columns to embed.}
\item{cols}{[NULL] A vector with a list of names of the columns to embed. If NULL, embed all columns of x.}
\item{dimension}{[1] The additional dimensions to generate when embedding.}
\item{take}{[1] Take from these dimensions only every take'th column.}
}

