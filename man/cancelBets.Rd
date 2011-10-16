\name{cancelBets}
\alias{cancelBets}
\title{cancelBets
}
\description{
Betfair cancelBets
}
\usage{
cancelBets(bets = list(), service = Exchange)
}
\arguments{
  \item{bets}{bets
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
function(bets=list(), service=Exchange)
{
  if(!is.list(bets)) {
    l <- list()
    for(x in bets){
      l[length(l)+1] <- list(CancelBets=list(betId=x))
    }
    names(l) <-rep( "CancelBets",length(l))
    bets <- l
  }
  v <- .bfapi(call("cancelBets", bets=bets), service=service)
  z <- .xmlp("errorCode", v)
  if(is.null(z)) return(v)
  if(z != "OK") return(z)
  x <- .xmlp("betResults", v)
  a1 <- unlist(strsplit(x,"><"))
  a2 <- a1[grep(".*>.*<.*",a1)]
  l  <- unique(sub("[ >].*","",a2))  # labels
  l <- sub("<","",l)
  v  <- sub(".*>(.*)<.*","\\1",a2)   # values
  m  <- matrix(v, ncol=length(l), byrow=T)
  colnames(m) <- l
  x <- as.data.frame(m, stringsAsFactors=FALSE)
# XXX column type conversion
  x
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
