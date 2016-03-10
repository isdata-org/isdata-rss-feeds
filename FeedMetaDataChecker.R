options(stringsAsFactors = FALSE)

library(XML)
library(RCurl)
df = read.csv("urls.txt", sep=" ", header = FALSE)
colnames(df) = c("url", "currentTitle")
df$feedTitle = ""
df$feedDescription = ""

for (i in c(1:nrow(df))){
  print(i)
  xml.url <- df$url[i]
  script  <- getURL(xml.url, 
                    .opts=curlOptions(followlocation=TRUE,cookiefile="nosuchfile"), 
                    httpheader = c('User-Agent' = "rss feed checker"))
  doc <- xmlParse(script)

  # If a default namespace is specified, then we need to update the xpath expressions to be able to handle it
  ns = xmlNamespaceDefinitions(doc, simplify = TRUE)
  
  if (any(names(ns) %in% "")){
    defaultNSLoc = which(names(ns) == "")
    names(ns)[defaultNSLoc] <- "defaultNS"
    
    feedTitle = getNodeSet(doc, "//rdf:RDF/defaultNS:channel/defaultNS:title", namespaces=ns)  
    description = getNodeSet(doc, "//rdf:RDF/defaultNS:channel/defaultNS:description", namespaces=ns)  
  } else {
    feedTitle = getNodeSet(doc, "//rss/channel/title/text()")  
    description = getNodeSet(doc, "//channel/description/text()")
  }
  
  if (length(feedTitle) > 0){
    df$feedTitle[i] = xmlValue(feedTitle[[1]])  
  }

  if (length(description) > 0){
    df$feedDescription[i] = xmlValue(description[[1]])  
  }
}