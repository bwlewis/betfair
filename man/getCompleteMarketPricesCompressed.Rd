\name{getCompleteMarketPricesCompressed}
\alias{getCompleteMarketPricesCompressed}
\title{getCompleteMarketPricesCompressed
}
\description{
Betfair getCompleteMarketPricesCompressed
}
\usage{
getCompleteMarketPricesCompressed(marketId, service = Exchange)
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
  x <- .xmlp("completeMarketPrices", v)
# NOTE:
# We temporarily ignore removed runner fields. XX may be OK to do this.
# Backslash-escaped colons are also not handled as indicated in the doc.
# But, otherwise this is OK.
  s <- unlist(strsplit(x, ":", fixed=TRUE))
  hdr <- unlist(strsplit(s[1], "~", fixed=TRUE))
  l <- list(
         marketID = as.integer(hdr[1]),
         inPlayDelay = as.integer(hdr[2])
       )

# Iterate over the runners (if any)
  j <- 2
  while(j<=length(s))
  {
    r <- unlist(strsplit(s[j], "|", fixed=TRUE))
    h <- unlist(strsplit(r[1], "~", fixed=TRUE))
    runner <- list(
      selectionId = as.integer(h[1]),
      OrderIndex  = as.integer(h[2]),
      TotalAmountMatched = as.numeric(h[3]),
      LastPriceMatched  = as.numeric(h[4]),
      Handicap = as.numeric(h[5]),
      ReductionFactor = as.numeric(h[6]),
      Vacant = as.logical(h[7]),
      AsianLineId = as.integer(h[8]),
      FarSPPrice = as.numeric(h[9]),
      NearSPPrice =  as.numeric(h[10]),
      ActualSPPrice = as.numeric(h[11])
    )
# Iterate over the prices (if any)
    k <- 2
    while(k<=length(r))
    {
      M <- c()
      p <- unlist(strsplit(r[k],"~", fixed=TRUE))
      for(kk in seq(1,length(p),by=5)) {
        M <- rbind(M, c(
                 Price = as.numeric(p[kk]),
                 TotalAvailableToBack = as.numeric(p[kk+1]),
                 TotalAvailableToLay = as.numeric(p[kk+2]),
                 TotalBSPLayLiability = as.numeric(p[kk+3]),
                 TotalBSPBackersStakeVolume = as.numeric(p[kk+4])
               ))
      }
      runner[[length(runner)+1]] <- M
      names(runner)[length(runner)] <- "prices"
      k <- k + 1
    }
    l[[length(l) + 1]] <- runner
    names(l)[length(l)] <- h[1]
    j <- j + 1
  }
  l
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
