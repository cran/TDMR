\name{makeTdmRandomSeed}
\alias{makeTdmRandomSeed}
\title{Factory method to make a function generating pseudo-random random number seeds.}
\description{Factory method to make a function generating pseudo-random random number seeds.}
\details{Create first an object of type \code{\link{makeTdmRandomSeed}} and then call the 
returned value of that object (a function) as many times as you like. (It is necessary
to create the function object first to have in its environment the private storage for 
the number of calls to that object.)}
\value{A function object which can be invoked without any arguments and returns
each time a different integer in 0...100001+nCall. This is true even if it is called
many times within the same second (where Sys.time() will return the same integer).
nCall is the number of calls to this function object.}
\seealso{\code{\link{tdmRandomSeed}}}
\author{Wolfgang Konen, Patrick Koch \email{wolfgang.konen@fh-koeln.de}}
\examples{tdmRandomSeed = makeTdmRandomSeed();
for (i in 1:10) print(c(as.integer(Sys.time()), tdmRandomSeed()));}

