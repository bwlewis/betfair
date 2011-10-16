\name{getMarketPrices}
\alias{getMarketPrices}
\title{getMarketPrices
}
\description{
Betfair getMarketPrices
}
\usage{
getMarketPrices(marketId, currencyCode = list(), service = Exchange)
}
\arguments{
  \item{marketId}{marketId
}
  \item{currencyCode}{currencyCode
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
function(marketId, currencyCode=list(), service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("marketPrices",v)
  tags <- c("bspMarket","currencyCode","delay","discountAllowed","lastRefresh","marketBaseRate","marketId","marketInfo","marketStatus","numberOfWinners","removedRunners","runnerPrices")
  market <- lapply(as.list(tags), .xmlp, x)
  tags <-  sub(".*:","",tags)
  names(market) <- tags
  mrp <- .xml2list(market$runnerPrices)
  names(mrp) <- c()
  mrp <- lapply(mrp, .xml2list)
  mrp <- lapply(mrp, function(x) {x$bestPricesToBack=.xmlarray2dataframe(x$bestPricesToBack);x$bestPricesToLay=.xmlarray2dataframe(x$bestPricesToLay);x})
  market$runnerPrices <- mrp
  market
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
