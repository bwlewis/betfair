\name{updateBet}
\alias{updateBet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
updateBet(bet, newBetPersistenceType = NULL, newPrice = NULL, newSize = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{bet}{
%%     ~~Describe \code{bet} here~~
}
  \item{newBetPersistenceType}{
%%     ~~Describe \code{newBetPersistenceType} here~~
}
  \item{newPrice}{
%%     ~~Describe \code{newPrice} here~~
}
  \item{newSize}{
%%     ~~Describe \code{newSize} here~~
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
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
