\name{login}
\alias{login}
\title{login
}
\description{
Betfair login
}
\usage{
login(username, password, productId)
}
\arguments{
  \item{username}{username
}
  \item{password}{password
}
  \item{productId}{productId
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
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
