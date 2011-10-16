\name{getDetailAvailableMktDepth}
\alias{getDetailAvailableMktDepth}
\title{getDetailAvailableMktDepth
}
\description{
Betfair getDetailAvailableMktDepth
}
\usage{
getDetailAvailableMktDepth(marketId, selectionId, currencyCode = NULL, asianLineId = NULL, locale = NULL, service = Exchange)
}
\arguments{
  \item{marketId}{marketId
}
  \item{selectionId}{selectionId
}
  \item{currencyCode}{currencyCode
}
  \item{asianLineId}{asianLineId
}
  \item{locale}{locale
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
function(
  marketId, selectionId,
  currencyCode=NULL,
  asianLineId=NULL,
  locale=NULL,
  service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("priceItems",v)
  .xmlarray2dataframe(x)
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
