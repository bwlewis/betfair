\name{getEvents}
\alias{getEvents}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getEvents(eventParentId = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eventParentId}{
%%     ~~Describe \code{eventParentId} here~~
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
function(eventParentId=-1)
{
  v <- .bfapi(match.call(), service=Global)
  if(z <- .xmlp("errorCode", v) != "OK") return(z)
  eventItems <- .xmlp("eventItems",v)
  if(nchar(eventItems)>0) eventItems <- .xmlarray2dataframe(eventItems)
  marketSummary <- .xmlp('marketItems',v)
  if(nchar(marketSummary)>0) {
    marketSummary <- .xmlarray2dataframe(marketSummary)
# Add classed time column for convenience (without tz)
    z <- c()
    tryCatch(
      z <- strptime(marketSummary$startTime,format="\%Y-\%m-\%dT\%H:\%M:\%OS"),
      error=function(e) invisible())
    marketSummary$start <- z
  }
  couponLinks <- .xmlp('couponLinks',v)
  if(nchar(couponLinks)>0) couponLinks <- .xmlarray2dataframe(couponLinks)
  x <- list(eventParentId=.xmlp('eventParentId',v),
            eventItems=eventItems,
            marketSummary=marketSummary,
            errorCode=.xmlp('errorCode',v),
            couponLinks=couponLinks)
  x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
