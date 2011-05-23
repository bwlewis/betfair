`login` <- function(username, password, productId)
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

`logout` <- function()
{
  x <- .bfapi(match.call())
  x <- .xmlp("errorCode", x)
  rm("sessionToken",envir=.bfenv)
  x
}

`getAccountFunds` <- function(service=Exchange)
{
  x <- .bfapi(match.call(), service=service)
  list(
    timestamp=.xmlp("timestamp", x),
    availBalance=as.numeric(.xmlp("availBalance", x)),
    balance=as.numeric(.xmlp("balance", x)),
    commissionRetain=as.numeric(.xmlp("commissionRetain", x)),
    creditLimit=as.numeric(.xmlp("creditLimit", x)),
    currentBetfairPoints=as.numeric(.xmlp("currentBetfairPoints", x)),
    expoLimit=as.numeric(.xmlp("expoLimit", x)),
    exposure=as.numeric(.xmlp("exposure", x)),
    holidaysAvailable=as.integer(.xmlp("holidaysAvailable", x))
  )
}

`getAllCurrencies` <- function()
{
  x <- .bfapi(match.call())
  x <- .xmlp('currencyItems', x)
  x <- .xmlarray2dataframe(x)
  x[,2] <- as.numeric(x[,2])
  x
}

`getAllEventTypes` <- function()
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

# Example
# getAllMarkets(eventTypeIds=list(int=7))
# XXX countries, from/to filtering not working yet... ???
`getAllMarkets` <- function(eventTypeIds=list(), countries=list(),fromDate=list(),toDate=list(), service=Exchange)
{
  if(length(fromDate)>0) 
    fromDate <- format(as.Date(fromDate),"%Y-%m-%dT%H:%M:%SZ")
  if(length(toDate)>0) 
    toDate <- format(as.Date(toDate),"%Y-%m-%dT%H:%M:%S")
  v <- .bfapi(call('getAllMarkets', 
                    eventTypeIds=eventTypeIds,
                    countries=countries,
                    fromDate=fromDate, toDate=toDate), service=service)
  if(z <- .xmlp("errorCode", v) != "OK") return(z)
  x <- .xmlp('marketData', v)
  x <- .compressed2dataframe(x, col.names=c("Market ID", "Market Name", "Market Type", "Market Status", "Event Date", "Menu Path", "Event Hierachy", "Bet Delay", "Exchange Id", "ISO3 Country Code", "Last Refresh", "Number of Runners", "Number of Winners", "Total Amount Matched", "BSP Market", "Turning In Play"), colClasses=c("integer", rep("character",3), "numeric", rep("character",3), "integer", "character", "numeric", "integer", "integer", "numeric", "character", "character"))
# XXX XXX dropping milliseconds here !!! Change this...
  x[,5] <- as.POSIXct(x[,5]/1000,origin="1970-01-01", tz="GMT")
  x[,11] <- as.POSIXct(x[,11]/1000,origin="1970-01-01", tz="GMT")
  rownames(x) <- x[,1]
  x
}

`getEvents` <- function(eventParentId=-1)
{
  v <- .bfapi(match.call(), service=Global)
  if(z <- .xmlp("errorCode", v) != "OK") return(z)
  eventItems <- .xmlp("eventItems",v)
  if(nchar(eventItems)>0) eventItems <- .xmlarray2dataframe(eventItems)
  marketSummary <- .xmlp('marketItems',v)
  if(nchar(marketSummary)>0) {
    marketSummary <- .xmlarray2dataframe(marketSummary)
# Add classed time column for convenience (without tz)
    z <- c()
    tryCatch(
      z <- strptime(marketSummary$startTime,format="%Y-%m-%dT%H:%M:%OS"),
      error=function(e) invisible())
    marketSummary$start <- z
  }
  couponLinks <- .xmlp('couponLinks',v)
  if(nchar(couponLinks)>0) couponLinks <- .xmlarray2dataframe(couponLinks)
  x <- list(eventParentId=.xmlp('eventParentId',v),
            eventItems=eventItems,
            marketSummary=marketSummary,
            errorCode=.xmlp('errorCode',v),
            couponLinks=couponLinks)
  x
}

`filterEvents` <- function(events, course, time=NULL)
{
  course <- paste("^",course,sep="")
  j <- foreach(x=course,.combine=c) %do% grep(x, names(courseAbbrev), ignore.case=TRUE)
  if(length(j)<1) return(c())
  course <- courseAbbrev[j]
print(course)
  j <- foreach(x=course,.combine=c) %do% grep(x, events$marketName, ignore.case=TRUE)
  events[j,]
}

`getActiveEventTypes` <- function()
{
  v <- .bfapi(match.call(), service=Global)
  if(z <- .xmlp("errorCode", v) != "OK") return(z)
  x <- .xmlp("eventTypeItems",v)
  if(nchar(x)>0) x <- .xmlarray2dataframe(x) 
  x
}

`getInPlayMarkets` <- function(service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  if(z <- .xmlp("errorCode", v) != "OK") return(z)
  x <- .xmlp("marketData",v)
  x <- .compressed2dataframe(x, col.names=c("Market ID", "Market Name", "Market Type", "Market Status", "Event Date", "Menu Path", "Event Hierachy", "Bet Delay", "Exchange Id", "ISO3 Country Code", "Last Refresh", "Number of Runners", "Number of Winners", "Total Amount Matched", "BSP Market", "Turning In Play"), colClasses=c("integer", rep("character",3), "numeric", rep("character",3), "integer", "character", "numeric", "integer", "integer", "numeric", "character", "character"))
# XXX XXX dropping milliseconds here !!! Change this...
  x[,5] <- as.POSIXct(x[,5]/1000,origin="1970-01-01", tz="GMT")
  x[,11] <- as.POSIXct(x[,11]/1000,origin="1970-01-01", tz="GMT")
  x
}

`getCompleteMarketPricesCompressed` <- function(marketId, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("completeMarketPrices", v)
# NOTE:
# We temporarily ignore removed runner fields. XX may be OK to do this.
# Backslash-escaped colons are also not handled as indicated in the doc.
# But, otherwise this is OK.
  s <- unlist(strsplit(x, ":", fixed=TRUE))
  hdr <- unlist(strsplit(s[1], "~", fixed=TRUE))
  l <- list(
         marketID = as.integer(hdr[1]),
         inPlayDelay = as.integer(hdr[2])
       )

# Iterate over the runners (if any)
  j <- 2
  while(j<=length(s))
  {
    r <- unlist(strsplit(s[j], "|", fixed=TRUE))
    h <- unlist(strsplit(r[1], "~", fixed=TRUE))
    runner <- list(
      selectionId = as.integer(h[1]),
      OrderIndex  = as.integer(h[2]),
      TotalAmountMatched = as.numeric(h[3]),
      LastPriceMatched  = as.numeric(h[4]),
      Handicap = as.numeric(h[5]),
      ReductionFactor = as.numeric(h[6]),
      Vacant = as.logical(h[7]),
      AsianLineId = as.integer(h[8]),
      FarSPPrice = as.numeric(h[9]),
      NearSPPrice =  as.numeric(h[10]),
      ActualSPPrice = as.numeric(h[11])
    )
# Iterate over the prices (if any)
    k <- 2
    while(k<=length(r))
    {
      M <- c()
      p <- unlist(strsplit(r[k],"~", fixed=TRUE))
      for(kk in seq(1,length(p),by=5)) {
        M <- rbind(M, c(
                 Price = as.numeric(p[kk]),
                 TotalAvailableToBack = as.numeric(p[kk+1]),
                 TotalAvailableToLay = as.numeric(p[kk+2]),
                 TotalBSPLayLiability = as.numeric(p[kk+3]),
                 TotalBSPBackersStakeVolume = as.numeric(p[kk+4])
               ))
      }
      runner[[length(runner)+1]] <- M
      names(runner)[length(runner)] <- "prices"
      k <- k + 1
    }
    l[[length(l) + 1]] <- runner
    names(l)[length(l)] <- h[1]
    j <- j + 1
  }
  l
}

`getMarketInfo` <- function(marketId, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("marketLite", v)
  x <- as.list(.xmlarray2dataframe(x))
  x$numberOfRunners <- as.integer(x$numberOfRunners)
  x$delay <- as.numeric(x$delay)
  x$reconciled <- ifelse(x$reconciled == "false",FALSE,TRUE)
  x
}

# example:
# x = getAllMarkets(eventTypeIds=list(int=7))
# v = getMarketTradedVolumeCompressed(x[1,1])
`getMarketTradedVolumeCompressed` <- function(marketId, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("tradedVolume", v)
  s <- unlist(strsplit(x, ":", fixed=TRUE))
  l <- list()
# Iterate over the runners (if any)
  j <- 2
  while(j<=length(s))
  {
    r <- unlist(strsplit(s[j], "|", fixed=TRUE))
    h <- unlist(strsplit(r[1], "~", fixed=TRUE))
    runner <- list(
      selectionId = as.integer(h[1]),
      AsianLineId  = as.integer(h[2]),
      ActualBSP = as.numeric(h[3]),
      TotalBSPBackMatchedAmount  = as.numeric(h[4]),
      TotalBSPLiabilityMatchedAmount = as.numeric(h[5])
    )
# Iterate over the traded amounts (if any)
    M <- c()
    k <- 2
    while(k<=length(r))
    {
      p <- unlist(strsplit(r[k],"~", fixed=TRUE))
      M <- rbind(M, c(odds=as.numeric(p[1]), TotalMatchedAmount=as.numeric(p[2])))
      k <- k + 1
    }
    runner[[length(runner)+1]] <- M
    names(runner)[length(runner)] <- "tradedAmounts"
    l[[length(l) + 1]] <- runner
    names(l)[[length(l)]] <- h[1]
    j <- j + 1
  }
  l
}

`newBet` <- function(asianLineId=0,
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

# Place one or more bets. The bets function argument is normally a list
# of bets, e.g.:
# bet = newBet(marketId=xxx, price=xxx, selectionId=xxx, etc...)
# placeBets(bets=list(PlaceBets=bet))
# But, we added an easy way to place a single bet too:
# placeBets(bet)
#
`placeBets` <- function(bets=list(), service=Exchange)
{
# Single bet convenience call:
  if(!is.list(bets[[1]])) {
    bets = list(PlaceBets=bets)
  }
# Argument not named as required
  if(length(names(bets)) < length(bets)) {
    names(bets) <- rep("PlaceBets",length(bets))
  }
  v <- .bfapi(call("placeBets", bets=bets), service=service)
  z <- .xmlp("errorCode", v)
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

`getBetLite` <- function(betId, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("betLite", v)
  tags <- .xmltags(x)
  betLite <- lapply(as.list(tags), .xmlp, x)
  tags <-  sub(".*:","",tags)
  names(betLite) <- tags
  betLite
}

# bets must be a list of CancelBets, each of which must contain a betId
# example:
# cancelBets(bets=list(CancelBets=list(betId=5)))
# Pretty ugly! So, if bets is not a list, we coerce into the above form.
`cancelBets` <- function(bets=list(), service=Exchange)
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

`getMarket` <- function(marketId, includeCouponLinks=FALSE, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("market",v)
  tags <- .xmltags(x, exclude=c(".*Runner", ".*EventId", "asianLineId","handicap","name","selectionId"))
  market <- lapply(as.list(tags), .xmlp, x)
  tags <-  sub(".*:","",tags)
  names(market) <- tags
  market$runners <- .xmlarray2dataframe(market$runners)
  market$eventHierarchy <- .xmlarray2dataframe(market$eventHierarchy)
# XXX Todo: type conversion of the market fields
  market
}

`keepAlive` <- function()
{
  v <- .bfapi(match.call(), service=Global)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  invisible()
}

`convertCurrency` <- function(amount, fromCurrency, toCurrency)
{
  v <- .bfapi(match.call(), service=Global)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  z <- .xmlp("convertedAmount", v)
  return(as.numeric(z))
}

`getMarketPrices` <- function(marketId, currencyCode=list(), service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("marketPrices",v)
  tags <- c("bspMarket","currencyCode","delay","discountAllowed","lastRefresh","marketBaseRate","marketId","marketInfo","marketStatus","numberOfWinners","removedRunners","runnerPrices")
  market <- lapply(as.list(tags), .xmlp, x)
  tags <-  sub(".*:","",tags)
  names(market) <- tags
  market
}

`getSilks` <- function(markets, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("marketDisplayDetails",v)
  x
}

`getMUBets` <- function(betIds=NULL,
                        betStatus="MU",
                        excludeLastSecond=NULL,
                        marketId=NULL,
                        matchedSince=NULL,
                        orderBy="BET_ID",
                        recordCount=200,
                        sortOrder="ASC",
                        startRecord=0, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("bets",v)
  x <- .xmlarray2dataframe(x)
  d <- withCallingHandlers(strptime(x$placedDate,format="%Y-%m-%dT%H:%M:%OS"),
         error=function(e) x$placedDate)
  x$placedDate <-  d
  d <- withCallingHandlers(strptime(x$matchedDate,format="%Y-%m-%dT%H:%M:%OS"),
         error=function(e) x$matchedDate)
  x$matchedDate <-  d
  x
}

`getBet` <- function(betId, service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("bet",v)
  x <- .xmlx("matches",x)
  y <- as.list(.xmlarray2dataframe(x$body))
  y$matches <- .xmlarray2dataframe(x$element)
  y
}

`getDetailAvailableMktDepth` <- function(
  marketId, selectionId,
  currencyCode=NULL,
  asianLineId=NULL,
  locale=NULL,
  service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("priceItems",v)
  .xmlarray2dataframe(x)
}

`getBetHistory` <- function(
  betTypesIncluded="S",
  detailed=TRUE,
  eventTypeIds=list(int=7),
  marketId=0,
  locale=NULL, timezone=NULL,
  marketTypesIncluded=list(MarketTypeEnum="O"),
  placedDateFrom="2011-01-01T01:00:00Z",
  placedDateTo=format(Sys.Date(),"%Y-%m-%dT%H:%M:%SZ"),
  recordCount=100,
  sortBetsBy="BET_ID",
  startRecord=1,
  service=Exchange)
{
  v <- .bfapi(match.call(), service=service, allowNull=TRUE)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xmlp("betHistoryItems",v)
  x <- .xml2list(x)
  names(x) <- c()
  x <- lapply(x,.xml2list)
  foreach(w=x) %do% {w$matches = .xmlarray2dataframe(w$matches);w}
}

`getMarketProfitAndLoss` <- function(
   marketID,
   includeSettledbets="N",
   includeBspBets="N",
   netOfCommision="N",
   service=Exchange)
{
  v <- .bfapi(match.call(), service=service)
  z <- .xmlp("errorCode", v)
  if(z != "OK") return(z)
  x <- .xml2list(v)
x
}
