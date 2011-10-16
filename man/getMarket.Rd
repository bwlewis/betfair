\name{getMarket}
\alias{getMarket}
\title{getMarket
}
\description{
Betfair getMarket
}
\usage{
getMarket(marketId, includeCouponLinks = FALSE, service = Exchange)
}
\arguments{
  \item{marketId}{marketId
}
  \item{includeCouponLinks}{includeCouponLinks
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
function(marketId, includeCouponLinks=FALSE, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("market",v)
  tags <- .xmltags(x, exclude=c(".*Runner", ".*EventId", "asianLineId","handicap","name","selectionId"))
  market <- lapply(as.list(tags), .xmlp, x)
  tags <-  sub(".*:","",tags)
  names(market) <- tags
  market$runners <- .xmlarray2dataframe(market$runners)
  market$eventHierarchy <- .xmlarray2dataframe(market$eventHierarchy)
# XXX Todo: type conversion of the market fields
  market
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
