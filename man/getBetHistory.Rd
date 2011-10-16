\name{getBetHistory}
\alias{getBetHistory}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getBetHistory(betTypesIncluded = "S", detailed = TRUE, eventTypeIds = list(int = 7), marketId = 0, locale = NULL, timezone = NULL, marketTypesIncluded = list(MarketTypeEnum = "O"), placedDateFrom = "2011-01-01T01:00:00Z", placedDateTo = format(Sys.Date(), "%Y-%m-%dT%H:%M:%SZ"), recordCount = 100, sortBetsBy = "BET_ID", startRecord = 1, service = Exchange)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{betTypesIncluded}{
%%     ~~Describe \code{betTypesIncluded} here~~
}
  \item{detailed}{
%%     ~~Describe \code{detailed} here~~
}
  \item{eventTypeIds}{
%%     ~~Describe \code{eventTypeIds} here~~
}
  \item{marketId}{
%%     ~~Describe \code{marketId} here~~
}
  \item{locale}{
%%     ~~Describe \code{locale} here~~
}
  \item{timezone}{
%%     ~~Describe \code{timezone} here~~
}
  \item{marketTypesIncluded}{
%%     ~~Describe \code{marketTypesIncluded} here~~
}
  \item{placedDateFrom}{
%%     ~~Describe \code{placedDateFrom} here~~
}
  \item{placedDateTo}{
%%     ~~Describe \code{placedDateTo} here~~
}
  \item{recordCount}{
%%     ~~Describe \code{recordCount} here~~
}
  \item{sortBetsBy}{
%%     ~~Describe \code{sortBetsBy} here~~
}
  \item{startRecord}{
%%     ~~Describe \code{startRecord} here~~
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
function(
  betTypesIncluded="S",
  detailed=TRUE,
  eventTypeIds=list(int=7),
  marketId=0,
  locale=NULL, timezone=NULL,
  marketTypesIncluded=list(MarketTypeEnum="O"),
  placedDateFrom="2011-01-01T01:00:00Z",
  placedDateTo=format(Sys.Date(),"\%Y-\%m-\%dT\%H:\%M:\%SZ"),
  recordCount=100,
  sortBetsBy="BET_ID",
  startRecord=1,
  service=Exchange)
{
  v <- .bfapi(match.call(), service=service, allowNull=TRUE)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("betHistoryItems",v)
  x <- .xml2list(x)
  names(x) <- c()
  x <- lapply(x,.xml2list)
  foreach(w=x) \%do\% {w$matches = .xmlarray2dataframe(w$matches);w}
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
