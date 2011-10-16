\name{getAllCurrencies}
\alias{getAllCurrencies}
\title{getAllCurrencies
}
\description{
Betfair getAllCurrencies
}
\usage{
getAllCurrencies()
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
function()
{
  x <- .bfapi(match.call())
  x <- .xmlp('currencyItems', x)
  x <- .xmlarray2dataframe(x)
  x[,2] <- as.numeric(x[,2])
  x
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
