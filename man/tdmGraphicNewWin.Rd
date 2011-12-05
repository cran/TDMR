\name{tdmGraphicNewWin}
\alias{tdmGraphicNewWin}
\title{Initialize a new window.}
\usage{tdmGraphicNewWin(opts, ...)
}
\description{Initialize a new window. Initialize a new window ("win")/ a new file ("png") for current graphic device.}
\arguments{\item{opts}{with \code{opts$GD.DEVICE} one out of [\code{"pdf"},\code{"png"},\code{"win"},\code{"non"}]}
\item{...}{optional arguments to hand over to \code{\link{png}} or \code{windows} or \code{X11} in 
package grDevices (the other devices require no further arguments)}
}

