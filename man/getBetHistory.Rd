\name{getBetHistory}
\alias{getBetHistory}
\title{getBetHistory
getBetHistory
}
\description{
}
\usage{
getBetHistory(betTypesIncluded = 'S', detailed = TRUE, eventTypeIds = list(int = 7), marketId = 0, locale = NULL, timezone = NULL, marketTypesIncluded = list(MarketTypeEnum = 'O'), placedDateFrom = '2011-01-01T01:00:00Z', placedDateTo = date, recordCount = 100, sortBetsBy = 'BET_ID', startRecord = 1, service = Exchange)
}
\arguments{
  \item{betTypesIncluded}{betTypesIncluded
}
  \item{detailed}{detailed
}
  \item{eventTypeIds}{eventTypeIds
}
  \item{marketId}{marketId
}
  \item{locale}{locale
}
  \item{timezone}{timezone
}
  \item{marketTypesIncluded}{marketTypesIncluded
}
  \item{placedDateFrom}{placedDateFrom
}
  \item{placedDateTo}{placedDateTo
}
  \item{recordCount}{recordCount
}
  \item{sortBetsBy}{sortBetsBy
}
  \item{startRecord}{startRecord
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
  betTypesIncluded="S",
  detailed=TRUE,
  eventTypeIds=list(int=7),
  marketId=0,
  locale=NULL, timezone=NULL,
  marketTypesIncluded=list(MarketTypeEnum="O"),
  placedDateFrom="2011-01-01T01:00:00Z",
  placedDateTo=format(Sys.Date(),"\%Y-\%m-\%dT\%H:\%M:\%SZ"),
  recordCount=100,
  sortBetsBy="BET_ID",
  startRecord=1,
  service=Exchange)
{
  v <- .bfapi(match.call(), service=service, allowNull=TRUE)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("betHistoryItems",v)
  x <- .xml2list(x)
  names(x) <- c()
  x <- lapply(x,.xml2list)
  foreach(w=x) \%do\% {w$matches = .xmlarray2dataframe(w$matches);w}
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
