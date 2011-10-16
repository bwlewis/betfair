\name{getMarketTradedVolumeCompressed}
\alias{getMarketTradedVolumeCompressed}
\title{getMarketTradedVolumeCompressed
}
\description{
Betfair getMarketTradedVolumeCompressed
}
\usage{
getMarketTradedVolumeCompressed(marketId, service = Exchange)
}
\arguments{
  \item{marketId}{marketId
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
function(marketId, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("tradedVolume", v)
  s <- unlist(strsplit(x, ":", fixed=TRUE))
  l <- list()
# Iterate over the runners (if any)
  j <- 2
  while(j<=length(s))
  {
    r <- unlist(strsplit(s[j], "|", fixed=TRUE))
    h <- unlist(strsplit(r[1], "~", fixed=TRUE))
    runner <- list(
      selectionId = as.integer(h[1]),
      AsianLineId  = as.integer(h[2]),
      ActualBSP = as.numeric(h[3]),
      TotalBSPBackMatchedAmount  = as.numeric(h[4]),
      TotalBSPLiabilityMatchedAmount = as.numeric(h[5])
    )
# Iterate over the traded amounts (if any)
    M <- c()
    k <- 2
    while(k<=length(r))
    {
      p <- unlist(strsplit(r[k],"~", fixed=TRUE))
      M <- rbind(M, c(odds=as.numeric(p[1]), TotalMatchedAmount=as.numeric(p[2])))
      k <- k + 1
    }
    runner[[length(runner)+1]] <- M
    names(runner)[length(runner)] <- "tradedAmounts"
    l[[length(l) + 1]] <- runner
    names(l)[[length(l)]] <- h[1]
    j <- j + 1
  }
  l
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
