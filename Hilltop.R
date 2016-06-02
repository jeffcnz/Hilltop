#Functions to interact with a Hilltop server and provide data in R data frames.
# Jeff Cooke May 2016

#Install the required packages
pkgs <- c('XML', 'reshape2', 'plyr')
if(!all(pkgs %in% installed.packages()[, 'Package']))
  install.packages(pkgs, dep = T)

require(XML)  
require(reshape2)
require(plyr)

#Get a list of sites available from a service
#May be able to tidy this and use similar method to getting measurement data.

hilltopSiteList <- function(endpoint) {
  
  url <- paste(endpoint, "?service=Hilltop&Request=SiteList", sep = "")
  sitexml <- xmlInternalTreeParse(url)
  s <- getNodeSet(sitexml, "//HilltopServer/Site")
  hillsites <- sapply(s, function(el) xmlGetAttr(el, "Name"))
  return(hillsites)
}


hilltopDsMeasList <- function(measlistxml) {
  #Helper function.
  #Takes an xml document from a Hilltop MeasurementList at a Site request. 
  #Returns a dataframe of the datasource information and measurements names.
  dstemp <- do.call(rbind, xpathApply(measlistxml, "/HilltopServer/DataSource", function(node) {
    xp <- "./*"
    datasource <- xmlGetAttr(node, "Name")
    type <- xpathSApply(node, "./TSType", xmlValue)
    datasourceid <- paste(type, datasource)
    attribute <- xpathSApply(node, xp, xmlName)
    value <- xpathSApply(node, xp, function(x) {
      if(xmlName(x) == "Measurement") {xmlGetAttr(x, "Name") } else {xmlValue(x) }
    } )
    data.frame(datasourceid, datasource, attribute, value, stringsAsFactors = FALSE)
  } ) )
  ds <- subset(dstemp, attribute != "Measurement")
  meas <- subset(dstemp, attribute == "Measurement", select = c("datasourceid", "value") )
  colnames(meas) [which(names(meas) == "value") ] <- "MeasurementName"
  castds <- dcast(ds, datasourceid + datasource ~ attribute, value.var = "value")
  castds <- merge(castds, meas, all = TRUE)
  castds <- subset(castds, select= -c(datasourceid) )
  
  return(castds)
}


hilltopMeasInfoList <- function(measlistxml) {
  #Helper function.
  #Takes an xml document from a Hilltop MeasurementList at a Site request. 
  #Returns a dataframe of the measurement information and datasources.
  dstemp <- do.call(rbind, xpathApply(measlistxml, "/HilltopServer/DataSource/Measurement", function(node) {
    xp <- "./*"
    datasource <- xpathSApply(node, "..", function(x) {xmlGetAttr(x, "Name") } )
    MeasurementName <- xmlGetAttr(node, "Name")
    measurementid <- paste(datasource, MeasurementName)
    attribute <- xpathSApply(node, xp, xmlName)
    value <- xpathSApply(node, xp, xmlValue) 
    data.frame(measurementid, datasource, MeasurementName, attribute, value, stringsAsFactors = FALSE)
  } ) )
  castmeas <- dcast(dstemp, measurementid + datasource + MeasurementName ~ attribute, value.var = "value")
  castmeas <- subset(castmeas, select = -c(measurementid) )
  
  return(castmeas)
}

hilltopDsMeasListFull <- function(measlistxml) {
  #Takes an xml document from a Hilltop MeasurementList at a Site request. 
  #Returns a dataframe of all of the datasource and measurement information combined.
  t<-hilltopDsMeasList(measlistxml)
  m<-hilltopMeasInfoList(measlistxml)
  full<-merge(t, m, by = c("datasource", "MeasurementName") , all = TRUE)
  return(full)
}


#working and test

#url<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Porangahau%20River%20at%20SH52%20Opposite%20Quarry&Measurement=WQ%20Sample&From=1/1/2004"
#url<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Porangahau River at SH52 Opposite Quarry&Measurement=Total Nitrogen&From=1/1/2004"
#url<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Esk%20River%20at%20Waipunga%20Bridge&Measurement=Total%20Nitrogen&From=1/1/2004"
#dataxml<-xmlParse(url)


hilltopValueHelper <- function(x) {
  #Helper function to return the appropriate xml value depending whether the value of interest is from a named node,
  #or is a named parameter.
  if(xmlName(x) != "T") {
    if(xmlName(x) == "Parameter") {
      return(xmlGetAttr(x, "Value"))
    } else {return(xmlValue(x)) }
  }
}


hilltopAttributeHelper <- function(x) {
  #Helper function to return the appropriate xml attribute name depending whether the attribute of interest is from a named node,
  #or is a named parameter.
  if(xmlName(x) != "T") {
    if(xmlName(x) == "Parameter") {
      return(xmlGetAttr(x, "Name"))
    } else {return(xmlName(x)) }
  }
}


hilltopMeasurementToDF <- function(dataxml) {
  #Helper function that reads the nodes within a the Measurement node of a Hilltop XML response
  #from a xmlParse(url) request such as dataxml<-xmlParse(url).
  #Returns a dataframe of the data for each timestamp.
  #Handles missing results and doen't require prior knowledge of parameter names.
  #Handles true measurements and WQ Sample requests
  idNodes <- getNodeSet(dataxml, "//Measurement/Data/E")
  Times <- lapply(idNodes, xpathApply, path = "./T", xmlValue)
  values <- lapply(idNodes, xpathApply, path = "./*", hilltopValueHelper)
  attributes <- lapply(idNodes, xpathApply, path = "./*", hilltopAttributeHelper)
  data <- do.call(rbind.data.frame, Reduce(function(x,y) Map(cbind, x, y), list(Times, attributes, values)))
  names(data) <- c("Time", "Attribute", "Content")
  data <- data[!(data$Attribute == "NULL"), ]
  data <- data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
  cdata <- dcast(data, Time ~ Attribute, value.var = "Content")
  cdata$Time <- as.POSIXct(strptime(cdata$Time, format = "%Y-%m-%dT%H:%M:%S"))
  return(cdata)
}


hilltopDataSourceToDF<-function(dataxml) {
  #Helper function that reads the nodes within a the DataSource ItemInfo node of a Hilltop XML response
  #from a xmlParse(url) request such as dataxml<-xmlParse(url).
  #Returns a dataframe of the Info for each Item.
  #Handles missing results and doen't require prior knowledge of the items.
  idNodes <- getNodeSet(dataxml, "//Measurement/DataSource")
  Item <- lapply(idNodes, xpathApply, path = "./ItemInfo", xmlGetAttr, "ItemNumber")
  values <- lapply(idNodes, xpathApply, path = "./ItemInfo/*", hilltopValueHelper)
  attributes <- lapply(idNodes, xpathApply, path = "./ItemInfo/*", hilltopAttributeHelper)
  data <- data.frame(Attribute = unlist(attributes), Content = unlist(values))
  data$Item <- unlist(Item)
  data <- data[!(data$Attribute == "NULL"), ]
  data <- data.frame(lapply(data, as.character), stringsAsFactors = FALSE)
  cdata <- dcast(data, Item ~ Attribute, value.var = "Content")
  return(cdata)
}


hilltopMeasurement<-function(dataxml){
  #Main function that converts a Hilltop XML document 
  #from a xmlParse(url) request such as dataxml<-xmlParse(url)
  #for a water quality measurement into a 
  #dataframe that contains the measurement information.
  #It returns a dataframe of the data for each timestamp, including DataSource Information and the Site Name.
  #This dataframe can be merged with a WQ Sample dataframe processed using xmlHilltopMeasurementToDF
  Site <- dataxml[["string(//Measurement/@SiteName)"]]
  df <- hilltopMeasurementToDF(dataxml)
  df$Site <- Site
  items <- hilltopDataSourceToDF(dataxml)
  df$Measurement <- items$ItemName
  df$Units <- if(is.null(items$Units)) {c("")} else {items$Units}
  return(df)
}



