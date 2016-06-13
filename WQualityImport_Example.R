#Jeff Cooke May 2016
# Goal is to build functions that import water quality data to a useable dataframe for plotting and analysis.  
# HBRC use Hilltop as their Time Series server and so the calls have been built for this server.
# This script has been developed as an example of using the Hilltop.R functions to import 
# Water Quality into R data frames.

#/* -===Include required function libraries===- */ 


#set the source of the Hilltop.R file, this will need editing and un commenting
#source("Hilltop.R")



#The monitoring sites reference data csv file mentioned below is a csv from a WFS import into excel, 
#this could be easily modified to read the WFS directly. 
#The lat long are extracted from the location information, this isn't required for the time series import
#but is for uploading data to the Shiny app for exploring data.

#monitoringsites<-read.csv("20160215MonitoringSitesReferenceData.csv", stringsAsFactors = FALSE)
#monitoringsites$lat<-substring(monitoringsites$ns2.pos,1,regexpr(' ', monitoringsites$ns2.pos))
#monitoringsites$long<-substring(monitoringsites$ns2.pos, regexpr(' ', monitoringsites$ns2.pos), nchar(monitoringsites$ns2.pos))
#swQualitySites<-subset(monitoringsites, ns3.SWQuality=="Yes")




#Bring in a site list and measurement list.
#For testing purposes examples are provided as vectors for the data frame.
WQSites<-NULL
WQMeasurements<-NULL
#WQSites<-read.csv("WQSites.csv", stringsAsFactors=FALSE) #Read sites in from csv
WQSites$Site<-c("Esk River at Waipunga Bridge", "Tukituki River at Red Bridge") #Provide sites so stand alone code


#WQSites$Site<-swQualitySites$ns3.CouncilSiteID

#WQMeasurements<-read.csv("WQMeasurements.csv", stringsAsFactors=FALSE) #Read sites in from csv
WQMeasurements$Measurements<-c("Total Phosphorus[Total Phosphorus]", "Dissolved Reactive Phosphorus", "Ammoniacal Nitrogen", "Total Nitrogen","Nitrite Nitrogen", "Nitrate Nitrogen", "Nitrate + Nitrite Nitrogen") #Provide measurements so stand alone code


#Set the time range for the requests, trimmed from full 10 years due to missing measurement and qc
startDate<-"1/1/2004"
endDate<-"1/1/2015"

#vendor <- "HILLTOP"


tss_url <- "http://data.hbrc.govt.nz/EnviroData/Emar.hts?"

ls<-length(WQSites$Site) #set ls to the number of sites that data is being requested for.
lm<-length(WQMeasurements$Measurements) # set lm to the number of measurements that data is being requested for.

output<-NULL # create an empty dataframe to append the results to for eventual output

for (s in 1:ls) { # start the loop for sites
  toutput<-NULL #create an empty dataframe for the temporary output (within the loop)
  site<-WQSites$Site[s] #select the site
  for (m in 1:lm) {  #start of the loop for measurements
    
    measurement<-WQMeasurements$Measurements[m] #select the measurement
    message(paste("Requesting", site, measurement))
    #build the request
    testrequest <- paste("service=Hilltop&request=GetData&Site=",site,"&Measurement=",measurement,"&From=",startDate,"&To=",endDate,sep="")
    #get the xml data from the server
    url<-paste(tss_url, testrequest, sep="")
    dataxml<-xmlParse(url)
    #convert the xml into a dataframe of measurement results
    #with basic error handling
    wqdata<-tryCatch({
      hilltopMeasurement(dataxml)
    }, error=function(err){message(paste("Error retrieving", site, measurement))})  
    
    toutput<-rbind(toutput,wqdata)#append the data to the dataframe called toutput (temporary dataframe)
  }
  #get the WQ Sample parameters for the site
  #build the request
  WQSampleRequest <- paste("service=Hilltop&request=GetData&Site=",site,"&Measurement=WQ Sample&From=",startDate,"&To=",endDate,sep="")
  #get the xml data from the server
  message(paste("Requesting", site, "WQ Sample"))
  url<-paste(tss_url, WQSampleRequest, sep="")
  wqdataxml<-xmlParse(url)
  
  ##convert the xml to a dataframe of WQ Sample results
  #with basic error handling added
  wqSampleData<-tryCatch({
    hilltopMeasurementToDF(wqdataxml)
  }, error=function(err){message(paste("Error retrieving", site, "WQ Sample Information"))})
  
  #merge the WQ Sample data with the measurement data with basic error handling.
  toutput<-tryCatch({
    merge(toutput,wqSampleData,by="Time",all.x=TRUE)
  }, error=function(err){message(paste("No WQ Sample information, leaving blank"))})
  output<-rbind(output,toutput) #append the data to the dataframe called output
  }


#Convert values to numbers and handle censured data
#TO DO: Put this in a function
#currently only dealing with less than figures, not greater than and simply halving less than figures as this is adequate for plotting

censured<-subset(output, substring(output$Value,1,1)=="<")

censured$valueprefix<-"<"
censured$result<-as.numeric(substring(censured$Value,2,nchar(censured$Value)))
censured$trendresult<-censured$result/2

nocensure<-subset(output, substring(output$Value,1,1)!="<")
nocensure$valueprefix<-"="
nocensure$result<-as.numeric(nocensure$Value)
nocensure$trendresult<-nocensure$result
output<-rbind(censured,nocensure)


#Test info, urls used for testing and debugging
testurl<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Mohaka River D/S Taharua River Confluence&measurement=Total Phosphorus[Total Phosphorus]&From=1/1/2004"
#testurl<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Porangahau River at SH52 Opposite Quarry&Measurement=WQ Sample&From=1/1/2004"
#testurl<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Awanui%20Stream%20at%20Flume&measurement=WQ%20Sample&From=1/1/2004"
testurl<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=MeasurementList&Site=Mohaka River D/S Taharua River Confluence"
testurl<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Ngaruroro River at Fernhill&Measurement=Total Nitrogen&From=1/6/2014"
testurl<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=GetData&Site=Ngaruroro River at Fernhill&Measurement=Flow[Water Level]&From=1/6/2016"
testurl<-"http://hilltopserver.horizons.govt.nz/data.hts?service=Hilltop&Request=MeasurementList&Site=Manawatu at Teachers College"
testurl<-"http://data.hbrc.govt.nz/EnviroData/Emar.hts?Service=Hilltop&Request=MeasurementList&Site=Aniwaniwa Park HQ"

dataxml<-xmlParse(testurl)
test1 <- hilltopMeasurement(dataxml)
