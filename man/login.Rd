\name{login}
\alias{login}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
login(username, password, productId)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{username}{
%%     ~~Describe \code{username} here~~
}
  \item{password}{
%%     ~~Describe \code{password} here~~
}
  \item{productId}{
%%     ~~Describe \code{productId} here~~
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
function(username, password, productId)
{
  if(exists('sessionToken', envir=.bfenv)) logout()
  h <- basicTextGatherer()
  body <- paste('<SOAP-ENV:Envelope xmlns:SOAP-ENV="http://schemas.xmlsoap.org/soap/envelope/" xmlns:SOAP-ENC="http://schemas.xmlsoap.org/soap/encoding/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"><SOAP-ENV:Body><m:login xmlns:m="http://www.Betfair.com/publicapi/v3/BFGlobalService/"><m:request><password>',password,'</password><productId>',productId,'</productId><username>',username,'</username><locationId>0</locationId><ipAddress>0</ipAddress></m:request></m:login></SOAP-ENV:Body></SOAP-ENV:Envelope>',sep="")
  curlPerform(url="https://api.betfair.com/global/v3/BFGlobalService", httpheader=c(Accept="text/xml", Accept="multipart/*", SOAPAction="https://api.betfair.com/global/v3/BFGlobalService", 'Content-Type' = "text/xml; charset=utf-8"),postfields=body, verbose=FALSE, writefunction = h$update)
  v <- h$value()
  if(.xmlp("errorCode", v) != "OK") return(FALSE)
  x <- .xmlp("sessionToken", v)
  assign("sessionToken", x, envir=.bfenv)
  TRUE
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
