#Functions to interact with a Hilltop server and provide data in R data frames.
# Jeff Cooke May 2016

#Install the required packages
pkgs <- c('XML', 'reshape2', 'plyr', 'RCurl')
if(!all(pkgs %in% installed.packages()[, 'Package']))
  install.packages(pkgs, dep = T)

require(XML)  
require(reshape2)
require(plyr)
require(RCurl)

#Get a list of sites available from a service
#May be able to tidy this and use similar method to getting measurement data.


hilltopSiteList <- function(sitexml) {
  #Takes an parsed xml document from a Hilltop SiteList request. 
  #Returns a dataframe of the available sites and location if available.
  stemp <- do.call(rbind, xpathApply(sitexml, "/HilltopServer/Site", function(node) {
    xp <- "./*"
    site <- xmlGetAttr(node, "Name")
    if(length(xmlChildren(node)) < 2) {
      attribute <- "NoLocation"
      value <- "NA"
    } else {
      attribute <- xpathSApply(node, xp, xmlName)
      value <- xpathSApply(node, xp, xmlValue) 
    }
    data.frame(site, attribute, value, stringsAsFactors = FALSE)
  } ) )
  castsite <- dcast(stemp, site ~ attribute, value.var = "value")
  if(!is.null(castsite$NoLocation)) {
    castsite <- subset(castsite, select = -c(NoLocation) )
  }
  
  
  return(castsite)
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

hilltopMeasInfoListExtra <- function(measlistxml) {
  #Helper function.
  #Takes an xml document from a Hilltop MeasurementList at a Site request. 
  #Returns a dataframe of the measurement information and datasource info.
  dstemp <- do.call(rbind, xpathApply(measlistxml, "/HilltopServer/DataSource/Measurement", function(node) {
    xp <- "./*"
    datasource <- xpathSApply(node, "..", function(x) {xmlGetAttr(x, "Name") } )
    MeasurementName <- xmlGetAttr(node, "Name")
    TSType <- xpathSApply(node, "../TSType", xmlValue)
    DataType <- xpathSApply(node, "../DataType", xmlValue)
    Interpolation <- xpathSApply(node, "../Interpolation", xmlValue)
    From <- xpathSApply(node, "../From", xmlValue)
    To <- xpathSApply(node, "../To", xmlValue)
    attribute <- xpathSApply(node, xp, xmlName)
    value <- xpathSApply(node, xp, xmlValue) 
    data.frame(datasource, MeasurementName, TSType, DataType, Interpolation, From, To, attribute, value, stringsAsFactors = FALSE)
  } ) )
  castmeas <- dcast(dstemp, datasource + TSType + DataType + Interpolation + From + To + MeasurementName ~ attribute, value.var = "value")
  
  return(castmeas)
}

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
  #from a anyXmlParse(url) request such as dataxml<-anyXmlParse(url).
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
  colnames(cdata)[colnames(cdata)=="I1"] <- "Value"
  return(cdata)
}


hilltopDataSourceToDF<-function(dataxml) {
  #Helper function that reads the nodes within a the DataSource ItemInfo node of a Hilltop XML response
  #from a anyXmlParse(url) request such as dataxml<-anyXmlParse(url).
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
  cdata <- if(length(data) > 0) {
    #If there is metadata then create a dataframe of it that can be associated
    #with the measurements
    dcast(data, Item ~ Attribute, value.var = "Content")
  } else {
    #Extract the measurement name from the xml and return this in a dataframe
    #that can be associated with the measuremnts.
    itemName <- dataxml[["string(//DataSource/@Name)"]]
    data.frame(ItemName = itemName)
  }
  return(cdata)
}


hilltopMeasurement<-function(dataxml){
  #Main function that converts a Hilltop XML document 
  #from a anyXmlParse(url) request such as dataxml<-anyXmlParse(url)
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

is.hilltopXml <- function(xmldata){
  #checks if an xml document is hilltop xml, returns True or False accordingly.
  server <- xmlName(xmlRoot(xmldata))
  if(length(grep("Hilltop",server))>0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

anyXmlParse <- function(url) {
  #Helper function to parse data from a hilltop server.
  #Takes a valid url as an input and returns a parsed xml document ready for other functions.
  #Handles https requests as well as http
  if(length(grep("https",url))>0){
    doc <- getURL(url, ssl.verifypeer = FALSE)
    return(xmlParse(doc))
  } else {return(xmlParse(url))}
}


hilltopEnsembleStatBkgnd<-function(dataxml){
  #Helper Function that takes the parsed xml from an Ensemble Statistics request.
  #Returns a single line dataframe of the STatistics background information
  #This needs to be combined with the stats themselves to get a full dataframe.
  bgtemp<-do.call(rbind, xpathApply(dataxml, "/HilltopServer", function(node) {
    xp <- "./*"
    attribute<-xpathSApply(node, xp, xmlName)
    value<-xpathSApply(node, xp, function(x){
      if(xmlName(x) %in% c("Hour", "Day", "Month")){xmlGetAttr(x,"Name")}else{xmlValue(x)}
    })
    data.frame(attribute, value, stringsAsFactors = FALSE)
  }))
  bgtemp<-subset(bgtemp, !attribute %in% c("Hour", "Day", "Month"))
  
  fintemp = setNames(data.frame(t(bgtemp[,-1])), bgtemp[,1])
  
  return(fintemp)
}


hilltopEnsembleStatByTimePeriod<-function(dataxml){
  #Helper function that takes parsed xml from an EnsembleStats Request.
  #Returns the statistics for each time period (depending whether hourly, monthly or annual stats)
  period <- function(dataxml) {
    #Helper function to determine what the measurement period of the EnsembleStats is.
    if (length(xpathApply(dataxml, "/HilltopServer/Hour", xmlGetAttr, "Hour"))>0) {
      return("Hour")} else if 
    (length(xpathApply(dataxml, "/HilltopServer/Day", xmlGetAttr, "Day"))>0) {
      return("Day")} else if (length(xpathApply(dataxml, "/HilltopServer/Month", xmlGetAttr, "Month"))>0) {
        return("Month")}
  }
  
  estatperiod<-period(dataxml)
  
  #Get the stats for each time period entry
  Statistic <- xpathApply(dataxml, "/HilltopServer/Statistic",  xmlValue)
  estat<-do.call(rbind, xpathApply(dataxml, paste("/HilltopServer/",estatperiod, sep=""), function(node) {
    xp <- "./*"
    periodID <- xpathSApply(node, ".", function(x){xmlGetAttr(x, "Name")})
    
    attribute<-xpathSApply(node, xp, xmlName)
    value<-xpathSApply(node, xp, xmlValue) 
    data.frame(periodID, attribute, value, stringsAsFactors = FALSE)
  }))
  
  estest<-dcast(estat, periodID ~ attribute, value.var= "value")
  estest$Statistic <- Statistic
  
  return(estest)
}

hilltopEnsembleStatFull <- function(dataxml) {
  #Takes the parsed xml from an EnsembleStats Request.
  #Returns a dataframe of the statistics for the period, along with the background information such as site measurement units etc.
  bg <- hilltopEnsembleStatBkgnd(dataxml)
  pe <- hilltopEnsembleStatByTimePeriod(dataxml)
  full<-merge(bg, pe, by = c("Statistic"), all = TRUE)
}
