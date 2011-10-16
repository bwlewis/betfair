\name{getMarketTradedVolumeCompressed}
\alias{getMarketTradedVolumeCompressed}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getMarketTradedVolumeCompressed(marketId, service = Exchange)
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
