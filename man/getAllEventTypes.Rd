\name{getAllEventTypes}
\alias{getAllEventTypes}
\title{getAllEventTypes
}
\description{
Betfair getAllEventTypes
}
\usage{
getAllEventTypes()
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
function()
{
  x <- .bfapi(match.call())

  x <- .xmlp('eventTypeItems', x)
  a1 <- unlist(strsplit(x,"><"))
  a2 <- a1[grep(".*>.*<.*",a1)]
  l  <- unique(sub("[ >].*","",a2))  # labels
  v  <- sub(".*>(.*)<.*","\\1",a2)   # values
  m  <- matrix(v, ncol=length(l), byrow=T)
  colnames(m) <- l
  x <- as.data.frame(m, stringsAsFactors=FALSE)
  rownames(x) <- as.character(x[,1])
  x[,1] <- as.integer(x[,1])
  x[,3] <- as.integer(x[,3])
  x[,4] <- as.integer(x[,4])
  x
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
