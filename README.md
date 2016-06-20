# Hilltop.R
##Background
The Hilltop.R file contains R functions for interacting with a [Hilltop](http://www.hilltop.co.nz) Time Series Server using Hilltop specific calls.
Hilltop servers can also provide some information in OGC compliant formats, basic documentation is available by entering a Hilltop Server [url](http://data.hbrc.govt.nz/EnviroData/Emar.hts?)

The file WQualityImport_Example.R demonstrates the use of some of the Hilltop.R functions for downloading water quality data from a Hilltop server, and contains some test urls. 
Note this example file was built before the helper function `anyXmlParse()` was written and the urls are http requests, hence it uses `xmlParse()` to parse the XML.  If you need to parse data from a secure server replace `xmlParse()` with `anyXmlParse()`

##Packages
The functions use the R packages XML, reshape2, plyr and RCurl.
These packages are checked for and loaded when the code is run.

##Using the code
Copy, download or fork the code.
Use `source(Hilltop.R)` to make the functions available.

##Functions
###hilltopSiteList
Given a server endpoint, requests and parses a SiteList request and generates a dataframe of sites.

###hilltopDsMeasList
_Helper function._
Takes a parsed xml document from a Hilltop MeasurementList at a Site request. 
Returns a dataframe of the datasource information and measurements names.

###hilltopMeasInfoList
_Helper function._
Takes an parsed xml document from a Hilltop MeasurementList at a Site request. 
Returns a dataframe of the measurement information and datasources.

###hilltopDsMeasListFull
Takes an xml document from a Hilltop MeasurementList at a Site request. 
Returns a dataframe of all of the datasource and measurement information combined.
_Occasional issues with some datasources and measurements, hilltopMeasInfoListExtra is another option._

###hilltopMeasInfoListExtra
_Helper function._
Takes an xml document from a Hilltop MeasurementList at a Site request. 
Returns a dataframe of the measurement information and some datasource information.
Similar to hilltopDsMeasListFull, but with limited datasource information.

###hilltopValueHelper
_Helper function_
Returns the appropriate xml value depending whether the value of interest is from a named node, or is a named parameter.

###hilltopAttributeHelper
_Helper function_
Returns the appropriate xml attribute name depending whether the attribute of interest is from a named node, or is a named parameter.

###hilltopMeasurementToDF
_Helper function_
Reads the nodes within a the Measurement node of a parsed Hilltop XML document from a request such as `dataxml<-anyXmlParse(url)`. 
Returns a dataframe of the data for each timestamp.
Handles missing results and doen't require prior knowledge of parameter names.
Handles true measurements and WQ Sample requests

###hilltopDataSourceToDF
_Helper function_
Reads the nodes within a the DataSource ItemInfo node of a parsed Hilltop XML document from a request such as `dataxml<-anyXmlParse(url)`.
Returns a dataframe of the Info for each Item.
Handles missing results and doen't require prior knowledge of the items.

###hilltopMeasurement
Main function that converts a Hilltop XML document from a request such as `dataxml<-anyXmlParse(url)` for a water quality measurement into a dataframe that contains the measurement information.
It returns a dataframe of the data for each timestamp, including DataSource Information and the Site Name.
This dataframe can be merged with a WQ Sample dataframe processed using hilltopMeasurementToDF

###is.hilltopXml
Function to check whether parsed xml is from a valid Hilltop Server.  Returns `TRUE` or `FALSE` accordingly.

###anyXmlParse
_Helper function_
Parses the xml from a url.  Checks whether the url is a secure server, if not uses `xmlParse` (XML Library).  If it is it uses RCurl functions to parse the data.

###hilltopEnsembleStatBkgnd
_Helper function_
Given the parsed xml from an Ensemble Statistics request.
Returns a single line dataframe of the STatistics background information
This needs to be combined with the stats themselves to get a full dataframe. see hilltopEnsembleStatsFull, and hilltopEnsembleStatsByTimePeriod

###hilltopEnsembleStatByTimePeriod
_Helper function_
Given the parsed xml from an EnsembleStats Request.
Returns the statistics for each time period (depending whether hourly, daily, or monthly stats)
 
###hilltopEnsembleStatFull
Takes the parsed xml from an EnsembleStats Request.
Returns a dataframe of the statistics for the period, along with the background information such as site, measurement, units and statistic.
 