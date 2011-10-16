\name{newBet}
\alias{newBet}
\title{newBet
}
\description{
Betfair newBet
}
\usage{
newBet(asianLineId = 0, betType = "B", betCategoryType = "E", betPersistenceType = "NONE", bspLiability = ifelse(betType == "B", 2, 10), marketId, price, selectionId, size = 2)
}
\arguments{
  \item{asianLineId}{asianLineId
}
  \item{betType}{betType
}
  \item{betCategoryType}{betCategoryType
}
  \item{betPersistenceType}{betPersistenceType
}
  \item{bspLiability}{bspLiability
}
  \item{marketId}{marketId
}
  \item{price}{price
}
  \item{selectionId}{selectionId
}
  \item{size}{size
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
function(asianLineId=0,
                     betType="B",
                     betCategoryType="E",
                     betPersistenceType="NONE",
                     bspLiability=ifelse(betType=="B",2.0, 10.0),
                     marketId,
                     price,
                     selectionId,
                     size=2.0)
{
# Add some checks here on size, types.
  bet <- list(asianLineId=asianLineId, betType=betType,
              betCategoryType=betCategoryType,
              betPersistenceType=betPersistenceType,
              bspLiability=bspLiability, marketId=marketId, price=price,
              selectionId=selectionId, size=size)
  return(bet)
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
