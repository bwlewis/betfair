\name{getMUBets}
\alias{getMUBets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getMUBets(betIds = NULL, betStatus = "MU", excludeLastSecond = NULL, marketId = NULL, matchedSince = NULL, orderBy = "BET_ID", recordCount = 200, sortOrder = "ASC", startRecord = 0, service = Exchange)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{betIds}{
%%     ~~Describe \code{betIds} here~~
}
  \item{betStatus}{
%%     ~~Describe \code{betStatus} here~~
}
  \item{excludeLastSecond}{
%%     ~~Describe \code{excludeLastSecond} here~~
}
  \item{marketId}{
%%     ~~Describe \code{marketId} here~~
}
  \item{matchedSince}{
%%     ~~Describe \code{matchedSince} here~~
}
  \item{orderBy}{
%%     ~~Describe \code{orderBy} here~~
}
  \item{recordCount}{
%%     ~~Describe \code{recordCount} here~~
}
  \item{sortOrder}{
%%     ~~Describe \code{sortOrder} here~~
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
function(betIds=NULL,
                        betStatus="MU",
                        excludeLastSecond=NULL,
                        marketId=NULL,
                        matchedSince=NULL,
                        orderBy="BET_ID",
                        recordCount=200,
                        sortOrder="ASC",
                        startRecord=0, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("bets",v)
  x <- .xmlarray2dataframe(x)
  d <- withCallingHandlers(strptime(x$placedDate,format="\%Y-\%m-\%dT\%H:\%M:\%OS"),
         error=function(e) x$placedDate)
  x$placedDate <-  d
  d <- withCallingHandlers(strptime(x$matchedDate,format="\%Y-\%m-\%dT\%H:\%M:\%OS"),
         error=function(e) x$matchedDate)
  x$matchedDate <-  d
  x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
