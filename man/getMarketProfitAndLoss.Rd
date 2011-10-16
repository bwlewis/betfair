\name{getMarketProfitAndLoss}
\alias{getMarketProfitAndLoss}
\title{getMarketProfitAndLoss
}
\description{
Betfair getMarketProfitAndLoss
}
\usage{
getMarketProfitAndLoss(marketID, includeSettledbets = "N", includeBspBets = "N", netOfCommision = "N", service = Exchange)
}
\arguments{
  \item{marketID}{marketID
}
  \item{includeSettledbets}{includeSettledbets
}
  \item{includeBspBets}{includeBspBets
}
  \item{netOfCommision}{netOfCommision
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
   marketID,
   includeSettledbets="N",
   includeBspBets="N",
   netOfCommision="N",
   service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xml2list(v)
x
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
