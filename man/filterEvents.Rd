\name{filterEvents}
\alias{filterEvents}
\title{filterEvents
}
\description{
Betfair filterEvents
}
\usage{
filterEvents(events, course, time = NULL)
}
\arguments{
  \item{events}{events
}
  \item{course}{course
}
  \item{time}{time
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
function(events, course, time=NULL)
{
  course <- paste("^",course,sep="")
  j <- foreach(x=course,.combine=c) \%do\% grep(x, names(courseAbbrev), ignore.case=TRUE)
  if(length(j)<1) return(c())
  course <- courseAbbrev[j]
print(course)
  j <- foreach(x=course,.combine=c) \%do\% grep(x, events$marketName, ignore.case=TRUE)
  events[j,]
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
