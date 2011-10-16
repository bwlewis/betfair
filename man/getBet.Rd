\name{getBet}
\alias{getBet}
\title{getBet
}
\description{
Betfair getBet
}
\usage{
getBet(betId, service = Exchange)
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
  x <- .xmlp("bet",v)
  x <- .xmlx("matches",x)
  y <- as.list(.xmlarray2dataframe(x$body))
  y$matches <- .xmlarray2dataframe(x$element)
  y
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
