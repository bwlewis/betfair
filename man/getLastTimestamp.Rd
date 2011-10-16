\name{getLastTimestamp}
\alias{getLastTimestamp}
\title{getLastTimestamp
}
\description{
Betfair getLastTimestamp
}
\usage{
getLastTimestamp(POSIX = FALSE)
}
\arguments{
  \item{POSIX}{POSIX
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
function(POSIX=FALSE)
{
  if(exists('timestamp',envir=.bfenv))
    if(POSIX) return(as.POSIXct(
        strptime(get('timestamp',envir=.bfenv),format="\%Y-\%m-\%dT\%H:\%M:\%OS")))
    else return(get('timestamp',envir=.bfenv))
  invisible()
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
