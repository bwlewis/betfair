\name{getBetLite}
\alias{getBetLite}
\title{getBetLite
}
\description{
Betfair getBetLite
}
\usage{
getBetLite(betId, service = Exchange)
}
\arguments{
  \item{betId}{betId
}
  \item{service}{service
}
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
function(betId, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("betLite", v)
  tags <- .xmltags(x)
  betLite <- lapply(as.list(tags), .xmlp, x)
  tags <-  sub(".*:","",tags)
  names(betLite) <- tags
  betLite
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
