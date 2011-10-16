\name{convertCurrency}
\alias{convertCurrency}
\title{convertCurrency
}
\description{
Betfair convertCurrency
}
\usage{
convertCurrency(amount, fromCurrency, toCurrency)
}
\arguments{
  \item{amount}{amount
}
  \item{fromCurrency}{fromCurrency
}
  \item{toCurrency}{toCurrency
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
function(amount, fromCurrency, toCurrency)
{
  v <- .bfapi(match.call(), service=Global)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  z <- .xmlp("convertedAmount", v)
  return(as.numeric(z))
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
