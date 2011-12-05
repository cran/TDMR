\name{tdmRandomSeed}
\alias{tdmRandomSeed}
\title{Generates pseudo-random random number seeds.}
\description{Generates pseudo-random random number seeds.}
\details{To use this mechanism, create first an object \code{tdmRandomSeed} with a call to 
\code{\link{makeTdmRandomSeed}}.}
\value{In each call to this function a different integer 
in 0...100001+nCall is returned. This is true even if it is called
many times within the same second (where Sys.time() will return the same integer).
nCall is the number of calls to this function object.}
\seealso{\code{\link{makeTdmRandomSeed}}}
\author{Wolfgang Konen, Patrick Koch \email{wolfgang.konen@fh-koeln.de}}
\examples{tdmRandomSeed = makeTdmRandomSeed();
for (i in 1:10) print(c(as.integer(Sys.time()), tdmRandomSeed()));}

