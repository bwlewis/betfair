\name{getMarketPrices}
\alias{getMarketPrices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getMarketPrices(marketId, currencyCode = list(), service = Exchange)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{marketId}{
%%     ~~Describe \code{marketId} here~~
}
  \item{currencyCode}{
%%     ~~Describe \code{currencyCode} here~~
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
function(marketId, currencyCode=list(), service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("marketPrices",v)
  tags <- c("bspMarket","currencyCode","delay","discountAllowed","lastRefresh","marketBaseRate","marketId","marketInfo","marketStatus","numberOfWinners","removedRunners","runnerPrices")
  market <- lapply(as.list(tags), .xmlp, x)
  tags <-  sub(".*:","",tags)
  names(market) <- tags
  mrp <- .xml2list(market$runnerPrices)
  names(mrp) <- c()
  mrp <- lapply(mrp, .xml2list)
  mrp <- lapply(mrp, function(x) {x$bestPricesToBack=.xmlarray2dataframe(x$bestPricesToBack);x$bestPricesToLay=.xmlarray2dataframe(x$bestPricesToLay);x})
  market$runnerPrices <- mrp
  market
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
