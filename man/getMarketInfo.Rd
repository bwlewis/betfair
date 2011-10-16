\name{getMarketInfo}
\alias{getMarketInfo}
\title{getMarketInfo
}
\description{
Betfair getMarketInfo
}
\usage{
getMarketInfo(marketId, service = Exchange)
}
\arguments{
  \item{marketId}{marketId
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
function(marketId, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("marketLite", v)
  x <- as.list(.xmlarray2dataframe(x))
  x$numberOfRunners <- as.integer(x$numberOfRunners)
  x$delay <- as.numeric(x$delay)
  x$reconciled <- ifelse(x$reconciled == "false",FALSE,TRUE)
  x
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
