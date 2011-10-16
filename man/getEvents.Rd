\name{getEvents}
\alias{getEvents}
\title{getEvents
}
\description{
Betfair getEvents
}
\usage{
getEvents(eventParentId = -1)
}
\arguments{
  \item{eventParentId}{eventParentId
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
function(eventParentId=-1)
{
  v <- .bfapi(match.call(), service=Global)
  if(z <- .xmlp("errorCode", v) != "OK") return(z)
  eventItems <- .xmlp("eventItems",v)
  if(nchar(eventItems)>0) eventItems <- .xmlarray2dataframe(eventItems)
  marketSummary <- .xmlp('marketItems',v)
  if(nchar(marketSummary)>0) {
    marketSummary <- .xmlarray2dataframe(marketSummary)
# Add classed time column for convenience (without tz)
    z <- c()
    tryCatch(
      z <- strptime(marketSummary$startTime,format="\%Y-\%m-\%dT\%H:\%M:\%OS"),
      error=function(e) invisible())
    marketSummary$start <- z
  }
  couponLinks <- .xmlp('couponLinks',v)
  if(nchar(couponLinks)>0) couponLinks <- .xmlarray2dataframe(couponLinks)
  x <- list(eventParentId=.xmlp('eventParentId',v),
            eventItems=eventItems,
            marketSummary=marketSummary,
            errorCode=.xmlp('errorCode',v),
            couponLinks=couponLinks)
  x
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
