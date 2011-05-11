# Constants and an environment to store package global state data:
.bfenv <- new.env(parent=emptyenv())
.bfenv$GlobalHeader <- 
'<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"> <soap:Body> <###REQUEST### xmlns="http://www.betfair.com/publicapi/v3/BFGlobalService/"><request><header><clientStamp>0</clientStamp><sessionToken>###SESSIONTOKEN###</sessionToken>'
.bfenv$ExchangeHeader <- 
'<soap:Envelope xmlns:soap="http://schemas.xmlsoap.org/soap/envelope/" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:xsd="http://www.w3.org/2001/XMLSchema"> <soap:Body> <###REQUEST### xmlns="http://www.betfair.com/publicapi/v5/BFExchangeService/"><request><header><clientStamp>0</clientStamp><sessionToken>###SESSIONTOKEN###</sessionToken>'
.bfenv$GlobalURL   <- "https://api.betfair.com/global/v3/BFGlobalService"
Global   <- "Global"
Exchange   <- "https://api.betfair.com/exchange/v5/BFExchangeService"
ExchangeAU <- "https://api-au.betfair.com/exchange/v5/BFExchangeService"

# Get a list of unique tags in the body, exluding patterns from the list:
.xmltags <- function(body, exclude=c())
{
  z <- unlist(strsplit(body,"</"))
  if(length(z)<2) return(NULL)
  z <- z[-1]
  z <- unique(sub(">.*","",z))
  for(x in exclude) {
    z <- sub(x,"",z)
  }
  z <- z[!nchar(z)==0]
  z
}

# Convert a rectangular array into a data frame
`.xmlarray2dataframe` <- function(x)
{
  if(is.character(x)) if(nchar(x)<1) return(NULL)
  if(length(x)<1) return(NULL)
  a1 <- unlist(strsplit(x,"><"))
  a2 <- a1[grep(".*>.*<.*",a1)]
  l  <- sub("^<","",a2)
  l  <- unique(sub("[ >].*","",l))  # labels
  l  <-  sub(".*:","",l)
  v  <- sub(".*>(.*)</.*","\\1",a2)   # values
  m  <- matrix(v, ncol=length(l), byrow=T)
  colnames(m) <- l
  as.data.frame(m, stringsAsFactors=FALSE)
}

# Greedily pick out one element from a body.
`.xmlp` <- function(tag, body)
{
  expr <- paste(".*<",tag,"[ >](.*)</",tag,">.*",sep="")
  if(is.na(grep(expr,body)[1])) return(NULL)
  a <- sub(expr, "\\1", body)
  j <- regexpr(">",a,fixed=TRUE)[[1]]
  if(j>0) a <- substring(a,j+1,nchar(a))
  a
}

# Reasonably generic conversion of XML to a list. Anything
# more serious than this should use the XML package, as this
# function is already quite slow and not as good as a real
# XML parser.
.xml2list <- function(x)
{
  p <- gregexpr("<[A-Z,a-z]",x)[[1]]
  if(length(p)<1) return(NULL)
  ret <- c()
  j <- 1
  while(j<=length(p)) {
    q <- gregexpr(">",substring(x,p[j]))[[1]]
    if(length(q)<1) break
    tag <- gsub("<(.*)>.*","\\1",substr(x,p[j],p[j]+q[1]-1))
    tag <- gsub(" xsi.*","",tag)
    term <- paste("</",tag,">",sep="")
    q <- gregexpr(term,substring(x,p[j]))[[1]]
    r <- gregexpr(">",substring(x,p[j]))[[1]]
    if(length(q)<0) break
    val <- substr(x,p[j]+r[1],p[j]+q[1]-2)
#    cat(j,p[j],q[1], tag, " ",val, "\n")
    k <- p[j] + q[1]
    j <- head(which(p>k),n=1)
    if(length(j)<1) break
    v <- list(val)
    names(v) <- tag
    ret <- c(ret, v)
  }
  ret
}

# Greedily extract one element from a body, returning the extracted
# element and the body less the extraction.
`.xmlx` <- function(tag, body)
{
  expr <- paste(".*<",tag,"[ >](.*)</",tag,">.*",sep="")
  expr1 <- paste("<",tag,"[ >](.*)</",tag,">",sep="")
  if(is.na(grep(expr,body)[1])) return(NULL)
  a <- sub(expr, "\\1", body)
  b <- sub(expr1, "", body)
  j <- regexpr(">",a,fixed=TRUE)[[1]]
  if(j>0) a <- substring(a,j+1,nchar(a))
  list(element=a,body=b)
}

`.list2xml` <- function(parameters)
{
  if(!is.list(parameters) || length(parameters)<1) return("")
  paste(lapply(1:length(parameters),
    function(j)
    {
      y <- eval(parameters[[j]])
      if(is.list(y)){
       if(length(y)<1) return("")
        paste("<",names(parameters)[[j]],">", .list2xml(y),
              "</", names(parameters)[[j]], ">", sep="")
      }
      else{
        tag <- ifelse(length(names(parameters)[[j]])>0,
                      names(parameters)[[j]], "item")
        paste("<",tag, ">",as.character(eval(parameters[[j]])),
              "</",tag,">",sep="")
      }
    })
  ,collapse="")
}

`.compressed2dataframe` <- function(x, col.names, colClasses=NA)
{
  f <- tempfile()
  x <- gsub(":", "\n", x)
  write(x, file=f)
  x <- read.table(f, sep="~", stringsAsFactors=TRUE, check.names=FALSE,
                  col.names=col.names, colClasses=colClasses, fill=TRUE)
  unlink(f)
  x
}


# Generic api interface -- the workhorse function of the package.
# This function attemps to map standard R function calls to the Betfair API.
`.bfapi` <- function(fncall, service=Global, debug=FALSE)
{
  a <- as.list(fncall)     # Get call and the argument list
  req <- a[[1]]            # The call name
  parameters <- a[-1]
  fmls <- formals(match.fun(req))
  if(length(parameters)>0) fmls[names(parameters)] <- parameters
  if(length(fmls)>0) parameters <- fmls[which(!unlist(lapply(fmls,is.null)))]
  j <- which(names(parameters)=='service')
  if(length(j)>0) parameters <- parameters[-j]
  if(!exists("sessionToken", envir=.bfenv))
    if(!debug) stop("Not logged in")
    else assign("sessionToken", "DEBUG", envir=.bfenv)
  st <- get("sessionToken", envir=.bfenv)
  if(is.null(st)) stop("Not logged in")
# Set up the appropriate header
  if(service == Global) {
    body <- .bfenv$GlobalHeader
    burl <- .bfenv$GlobalURL
  }
  else if(service == Exchange || service == ExchangeAU) {
    body <- .bfenv$ExchangeHeader
    burl <- service
  }
  else stop("Unknown service")
  body <- sub("###REQUEST###", req, body)
  body <- sub("###SESSIONTOKEN###", st, body)
  body <- paste(body, '</header>')
# Add the parameters (if any)
  if(!is.null(parameters) && length(parameters)>0) 
    body <- paste(body, .list2xml(parameters), sep="")
# Complete the document
  body <- paste(body, '</request> </',req,
                      '> </soap:Body> </soap:Envelope>', sep="")
  if(debug)  return(body)
  h <- basicTextGatherer()
  curlPerform(url=burl,
              httpheader=c(Accept="text/xml", Accept="multipart/*",
                           SOAPAction=burl,
                           'Content-Type' = "text/xml; charset=utf-8"),
              postfields=body,
              verbose=FALSE,
              writefunction = h$update
             )
  ret <- h$value()
# Check for updated session token:
  x <- .xmlp("sessionToken", ret)
  if(!is.null(x) && nchar(x)>3 && x != st)
    assign("sessionToken", x, envir=.bfenv)
  return(ret)
}
