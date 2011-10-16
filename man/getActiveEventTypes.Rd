\name{getActiveEventTypes}
\alias{getActiveEventTypes}
\title{getActiveEventTypes
}
\description{
Betfair getActiveEventTypes
}
\usage{
getActiveEventTypes()
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
  v <- .bfapi(match.call(), service=Global)
  if(z <- .xmlp("errorCode", v) != "OK") return(z)
  x <- .xmlp("eventTypeItems",v)
  if(nchar(x)>0) x <- .xmlarray2dataframe(x) 
  x
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
