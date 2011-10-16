\name{getAllMarkets}
\alias{getAllMarkets}
\title{getAllMarkets
}
\description{
Betfair getAllMarkets
}
\usage{
getAllMarkets(eventTypeIds = list(), countries = list(), fromDate = list(), toDate = list(), service = Exchange)
}
\arguments{
  \item{eventTypeIds}{eventTypeIds
}
  \item{countries}{countries
}
  \item{fromDate}{fromDate
}
  \item{toDate}{toDate
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
function(eventTypeIds=list(), countries=list(),fromDate=list(),toDate=list(), service=Exchange)
{
  if(length(fromDate)>0) 
    fromDate <- format(as.Date(fromDate),"\%Y-\%m-\%dT\%H:\%M:\%SZ")
  if(length(toDate)>0) 
    toDate <- format(as.Date(toDate),"\%Y-\%m-\%dT\%H:\%M:\%S")
  v <- .bfapi(call('getAllMarkets', 
                    eventTypeIds=eventTypeIds,
                    countries=countries,
                    fromDate=fromDate, toDate=toDate), service=service)
  if(z <- .xmlp("errorCode", v) != "OK") return(z)
  x <- .xmlp('marketData', v)
  x <- .compressed2dataframe(x, col.names=c("Market ID", "Market Name", "Market Type", "Market Status", "Event Date", "Menu Path", "Event Hierachy", "Bet Delay", "Exchange Id", "ISO3 Country Code", "Last Refresh", "Number of Runners", "Number of Winners", "Total Amount Matched", "BSP Market", "Turning In Play"), colClasses=c("integer", rep("character",3), "numeric", rep("character",3), "integer", "character", "numeric", "integer", "integer", "numeric", "character", "character"))
# XXX XXX dropping milliseconds here !!! Change this...
  x[,5] <- as.POSIXct(x[,5]/1000,origin="1970-01-01", tz="GMT")
  x[,11] <- as.POSIXct(x[,11]/1000,origin="1970-01-01", tz="GMT")
  rownames(x) <- x[,1]
  x
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
