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

hillSiteList<-function(endpoint){
  
  url<-paste(endpoint,"?service=Hilltop&Request=SiteList", sep="")
  sitexml<-xmlInternalTreeParse(url)
  s<-getNodeSet(sitexml, "//HilltopServer/Site")
  hillsites<-sapply(s,function(el) xmlGetAttr(el, "Name"))
  return(hillsites)
}

#helper function to get a dataframe of datasources available at a site, given an xml document from a measurementlist request.
#May be able to tidy this and use similar method to getting measurement data.
dataSourcesAtSite<-function(parsedxml){
  dsheaders<-c("NumItems", "TSType", "DataType", "Interpolation", "ItemFormat", "From", "To")
  
  
  datasourcename<-xmlSApply(getNodeSet(measxml, "//DataSource"), function(el) xmlGetAttr(el, "Name"))
  datasources<-data.frame(datasourcename)
  for(h in 1:length(dsheaders)){
    col<-dsheaders[h]
    datasources[[col]]<-xmlSApply(getNodeSet(measxml, paste("//DataSource/",col,sep="")), xmlValue)
    
  }
  return(datasources)
}

#Get a list of measurements available at a site
#May be able to tidy this and use similar method to getting measurement data.

measAtSite<-function(endpoint, site){
  url<-paste(endpoint,"?service=Hilltop&Request=MeasurementList&Site=",site, sep="")
  measxml<-xmlInternalTreeParse(url)
  d<-getNodeSet(measxml, "//HilltopServer/DataSource")
  #The datasource provides information about the measurement that will be useful.
  #Bring in the datasource information first as each datasource can have multiple measurements
  datasourcename<-dataSourcesAtSite(measxml)
  
  #There can be multiple measurements associated with a datasource.
  measheaders<-c("Item", "DefaultMeasurement", "RequestAs", "Units","Format")  
  #need to work through the datasources and get measurements for each, but qualityseries don't have measurements
  for(ds in 1:length(d)){
    measurementname<-xmlSApply(getNodeSet(measxml, paste("//DataSource[",ds,"]/Measurement",sep="")), function(el) xmlGetAttr(el, "Name"))
    temp<-data.frame(measurementname)
    temp$datasourcename<-xmlSApply(getNodeSet(measxml, paste("//DataSource[",ds,"]",sep="")), function(el) xmlGetAttr(el, "Name"))
    for(mh in 1:length(measheaders)){
      mcol<-measheaders[mh]
      temp[[mcol]]<-xmlSApply(getNodeSet(measxml, paste("//DataSource[",ds,"]/Measurement/",mcol,sep="")), xmlValue)
      
    if(ds==1){
      measurements<-temp
    }else{
      measurements<-rbind(measurements, temp)
    }  
    }
    
    
  }
  
  output<-merge(datasources, measurements)
  #sitemeas<-sapply(m,function(el) xmlGetAttr(el, "Name"))
  return(output)
  
  
}


#working and test

#url<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Porangahau%20River%20at%20SH52%20Opposite%20Quarry&Measurement=WQ%20Sample&From=1/1/2004"
#url<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Porangahau River at SH52 Opposite Quarry&Measurement=Total Nitrogen&From=1/1/2004"
#url<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Esk%20River%20at%20Waipunga%20Bridge&Measurement=Total%20Nitrogen&From=1/1/2004"
#dataxml<-xmlParse(url)

#Helper function to return the appropriate xml value depending whether the value of interest is from a named node,
#or is a named parameter.
xmlHilltopValueHelper<-function(x){
  if(xmlName(x) != "T"){
    if(xmlName(x) == "Parameter"){
      return(xmlGetAttr(x, "Value"))
    }else{return(xmlValue(x))}
  }
}

#Helper function to return the appropriate xml attribute name depending whether the attribute of interest is from a named node,
#or is a named parameter.
xmlHilltopAttributeHelper<-function(x){
  if(xmlName(x) != "T"){
    if(xmlName(x) == "Parameter"){
      return(xmlGetAttr(x, "Name"))
    }else{return(xmlName(x))}
  }
}

#Helper function that reads the nodes within a the Measurement node of a Hilltop XML response
#from a xmlParse(url) request such as dataxml<-xmlParse(url).
#Returns a dataframe of the data for each timestamp.
#Handles missing results and doen't require prior knowledge of parameter names.
#Handles true measurements and WQ Sample requests
#To do, add a subsetting option so only some columns are returned.
xmlHilltopMeasurementToDF<-function(dataxml){
  idNodes <- getNodeSet(dataxml, "//Measurement/Data/E")
  Times<-lapply(idNodes,xpathApply,path = "./T", xmlValue)
  values <- lapply(idNodes, xpathApply, path = "./*", xmlHilltopValueHelper)
  attributes <- lapply(idNodes, xpathApply, path = "./*", xmlHilltopAttributeHelper)
  #Intermittent issue with below, only for some sites and measurments!
  #data<-do.call(rbind.data.frame, mapply(cbind, Times, attributes, values))
  #changed above line to below
  data<-do.call(rbind.data.frame, Reduce(function(x,y) Map(cbind, x, y),list(Times, attributes,values)))
  names(data)<-c("Time", "Attribute", "Content")
  data<-data[!(data$Attribute == "NULL"), ]
  data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  cdata<-dcast(data, Time ~ Attribute, value.var= "Content")
  cdata$Time<-as.POSIXct(strptime(cdata$Time, format="%Y-%m-%dT%H:%M:%S"))
  return(cdata)
}

#Helper function that reads the nodes within a the DataSource ItemInfo node of a Hilltop XML response
#from a xmlParse(url) request such as dataxml<-xmlParse(url).
#Returns a dataframe of the Info for each Item.
#Handles missing results and doen't require prior knowledge of the items.
#To do, add a subsetting option so only some columns are returned.
xmlHilltopDataSourceToDF<-function(dataxml){
  idNodes <- getNodeSet(dataxml, "//Measurement/DataSource")
  Item<-lapply(idNodes,xpathApply,path = "./ItemInfo", xmlGetAttr, "ItemNumber")
  values <- lapply(idNodes, xpathApply, path = "./ItemInfo/*", xmlHilltopValueHelper)
  attributes <- lapply(idNodes, xpathApply, path = "./ItemInfo/*", xmlHilltopAttributeHelper)
  data<-data.frame(Attribute=unlist(attributes), Content=unlist(values))
  data$Item<-unlist(Item)
  data<-data[!(data$Attribute == "NULL"), ]
  data<-data.frame(lapply(data, as.character), stringsAsFactors=FALSE)
  cdata<-dcast(data, Item ~ Attribute, value.var= "Content")
  return(cdata)
}

#Main function that converts a Hilltop XML document 
#from a xmlParse(url) request such as dataxml<-xmlParse(url)
#for a water quality measurement into a 
#dataframe that contains the measurement information.
#It returns a dataframe of the data for each timestamp, including DataSource Information and the Site Name.
#This dataframe can be merged with a WQ Sample dataframe processed using xmlHilltopMeasurementToDF
xmlHilltopMeasurement<-function(dataxml){
  Site<-dataxml[["string(//Measurement/@SiteName)"]]
  df<-xmlHilltopMeasurementToDF(dataxml)
  df$Site<-Site
  items<-xmlHilltopDataSourceToDF(dataxml)
  df$Measurement<-items$ItemName
  df$Units<-if(is.null(items$Units)){c("")}else{items$Units}
  return(df)
}



