\name{getInPlayMarkets}
\alias{getInPlayMarkets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getInPlayMarkets(service = Exchange)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function(service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  if(z <- .xmlp("errorCode", v) != "OK") return(z)
  x <- .xmlp("marketData",v)
  x <- .compressed2dataframe(x, col.names=c("Market ID", "Market Name", "Market Type", "Market Status", "Event Date", "Menu Path", "Event Hierachy", "Bet Delay", "Exchange Id", "ISO3 Country Code", "Last Refresh", "Number of Runners", "Number of Winners", "Total Amount Matched", "BSP Market", "Turning In Play"), colClasses=c("integer", rep("character",3), "numeric", rep("character",3), "integer", "character", "numeric", "integer", "integer", "numeric", "character", "character"))
# XXX XXX dropping milliseconds here !!! Change this...
  x[,5] <- as.POSIXct(x[,5]/1000,origin="1970-01-01", tz="GMT")
  x[,11] <- as.POSIXct(x[,11]/1000,origin="1970-01-01", tz="GMT")
  x
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
