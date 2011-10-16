\name{updateBet}
\alias{updateBet}
\title{updateBet
}
\description{
Betfair updateBet
}
\usage{
updateBet(bet, newBetPersistenceType = NULL, newPrice = NULL, newSize = NULL)
}
\arguments{
  \item{bet}{bet
}
  \item{newBetPersistenceType}{newBetPersistenceType
}
  \item{newPrice}{newPrice
}
  \item{newSize}{newSize
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
function(bet, newBetPersistenceType=NULL, newPrice=NULL, newSize=NULL)
{
  if(is.numeric(bet) || is.character(bet)) bet <- getBet(betId=eval(bet))
  if(is.null(newBetPersistenceType)) newBetPersistenceType <- bet$persistenceType
  if(is.null(newPrice)) newPrice <- bet$price
  if(is.null(newSize)) newSize <- bet$remainingSize
  list(betId=bet$betId, newBetPersistenceType=newBetPersistenceType,
       newPrice=newPrice, newSize=newSize,
       oldBetPersistenceType=bet$betPersistenceType,
       oldPrice=bet$price,
       oldSize=bet$requestedSize)
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
