\name{getMUBets}
\alias{getMUBets}
\title{getMUBets
}
\description{
Betfair getMUBets
}
\usage{
getMUBets(betIds = NULL, betStatus = "MU", excludeLastSecond = NULL, marketId = NULL, matchedSince = NULL, orderBy = "BET_ID", recordCount = 200, sortOrder = "ASC", startRecord = 0, service = Exchange)
}
\arguments{
  \item{betIds}{betIds
}
  \item{betStatus}{betStatus
}
  \item{excludeLastSecond}{excludeLastSecond
}
  \item{marketId}{marketId
}
  \item{matchedSince}{matchedSince
}
  \item{orderBy}{orderBy
}
  \item{recordCount}{recordCount
}
  \item{sortOrder}{sortOrder
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
function(betIds=NULL,
                        betStatus="MU",
                        excludeLastSecond=NULL,
                        marketId=NULL,
                        matchedSince=NULL,
                        orderBy="BET_ID",
                        recordCount=200,
                        sortOrder="ASC",
                        startRecord=0, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("bets",v)
  x <- .xmlarray2dataframe(x)
  d <- withCallingHandlers(strptime(x$placedDate,format="\%Y-\%m-\%dT\%H:\%M:\%OS"),
         error=function(e) x$placedDate)
  x$placedDate <-  d
  d <- withCallingHandlers(strptime(x$matchedDate,format="\%Y-\%m-\%dT\%H:\%M:\%OS"),
         error=function(e) x$matchedDate)
  x$matchedDate <-  d
  x
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
