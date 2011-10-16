\name{getCompleteMarketPricesCompressed}
\alias{getCompleteMarketPricesCompressed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getCompleteMarketPricesCompressed(marketId, service = Exchange)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{marketId}{
%%     ~~Describe \code{marketId} here~~
}
  \item{service}{
%%     ~~Describe \code{service} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
