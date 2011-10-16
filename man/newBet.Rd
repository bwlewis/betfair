\name{newBet}
\alias{newBet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
newBet(asianLineId = 0, betType = "B", betCategoryType = "E", betPersistenceType = "NONE", bspLiability = ifelse(betType == "B", 2, 10), marketId, price, selectionId, size = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{asianLineId}{
%%     ~~Describe \code{asianLineId} here~~
}
  \item{betType}{
%%     ~~Describe \code{betType} here~~
}
  \item{betCategoryType}{
%%     ~~Describe \code{betCategoryType} here~~
}
  \item{betPersistenceType}{
%%     ~~Describe \code{betPersistenceType} here~~
}
  \item{bspLiability}{
%%     ~~Describe \code{bspLiability} here~~
}
  \item{marketId}{
%%     ~~Describe \code{marketId} here~~
}
  \item{price}{
%%     ~~Describe \code{price} here~~
}
  \item{selectionId}{
%%     ~~Describe \code{selectionId} here~~
}
  \item{size}{
%%     ~~Describe \code{size} here~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
