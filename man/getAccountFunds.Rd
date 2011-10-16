\name{getAccountFunds}
\alias{getAccountFunds}
\title{getAccountFunds
}
\description{
Betfair getAccountFunds
}
\usage{
getAccountFunds(service = Exchange)
}
\arguments{
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
function(service=Exchange)
{
  x <- .bfapi(match.call(), service=service)
  list(
    timestamp=.xmlp("timestamp", x),
    availBalance=as.numeric(.xmlp("availBalance", x)),
    balance=as.numeric(.xmlp("balance", x)),
    commissionRetain=as.numeric(.xmlp("commissionRetain", x)),
    creditLimit=as.numeric(.xmlp("creditLimit", x)),
    currentBetfairPoints=as.numeric(.xmlp("currentBetfairPoints", x)),
    expoLimit=as.numeric(.xmlp("expoLimit", x)),
    exposure=as.numeric(.xmlp("exposure", x)),
    holidaysAvailable=as.integer(.xmlp("holidaysAvailable", x))
  )
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
