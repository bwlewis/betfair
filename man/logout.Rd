\name{logout}
\alias{logout}
\title{logout
}
\description{
Betfair logout
}
\usage{
logout()
}
\details{
}
\value{
}
\references{
}
\author{
colin@betwise.co.uk
}
\note{
}


\seealso{
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function()
{
  x <- .bfapi(match.call())
  x <- .xmlp("errorCode", x)
  rm("sessionToken",envir=.bfenv)
  x
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
