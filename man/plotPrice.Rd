\name{plotPrice}
\alias{plotPrice}
\title{plotPrice
}
\description{
Betfair plotPrice
}
\usage{
plotPrice(R, width = 10)
}
\arguments{
  \item{R}{R
}
  \item{width}{width
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
function(R, width=10)
{
  P <- R
  if(is.list(R)) P <- R$prices
  f = as.factor(P[,1])
  back = c(P[,2])
  names(back)=f
  lay = c(P[,3])
  names(lay) = f
  p1 = par(mfcol=c(1,2))
  idx = back == 0 & lay == 0
  back = back[!idx]
  lay  = lay[!idx]
  ladder = names(lay)
  layPriceIndex = which(lay>0,arr.ind=T)[[1]]
  backPriceIndex = layPriceIndex - which(back[layPriceIndex:1]<1)[[1]]
  bcp = ladder[backPriceIndex]
  lcp = ladder[layPriceIndex]
  minIndex = max(layPriceIndex - width, 1)
  maxIndex = min(backPriceIndex + width, length(ladder))
  idx = seq(minIndex, maxIndex, by=1)
  back = back[idx]
  lay  = lay[idx]
  ladder = ladder[idx]
  ymax = par('usr')[[4]] - par('usr')[[3]]
  l = length(back)
# The following are taken from barplot.R:
  space = 0.2
  width = rep(1,l)
  delta = width/2
  w.r = cumsum(space + width)
  w.m = w.r - delta
  mx = max(max(back),max(lay))
  mn = min(min(back), min(lay))
  title = paste("To back current price",bcp)
  subtl = ifelse(is.list(R),paste("Total amount matched",R$TotalAmountMatched),
                  "")
  barplot(back,col=4,las=1,main=title,sub=subtl,horiz=T,yaxt="n",xlim=c(mx,mn))
  axis(side=4,labels=names(back),at=w.m,las=1,cex.axis=0.8)
  p2=par(mar=c(5,1,4,2))
  title = paste("To lay current price",lcp)
  subtl = ifelse(is.list(R),paste("Last price matched",R$LastPriceMatched), "")
  barplot(lay,col=6,las=1,main=title,sub=subtl,horiz=T,yaxt="n",xlim=c(mn,mx))
  axis(side=2,labels=rep("",l),at=w.m,las=1,cex.axis=0.8)
  par(p2)
  par(p1)
  }
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
