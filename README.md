# Hilltop
R functions for interacting with a Hilltop Time Series Server

The file Hilltop.R contains functions for interacting with a Hilltop server.

The file WQualityImport_Example.R demonstrates the use of some of the Hilltop.R functions for downloading water quality data from a Hilltop server. 

Hilltop.R

Functions

hilltopSiteList
Given a server endpoint, requests and parses a SiteList request and generates a dataframe of sites.

hilltopDsMeasList
Helper function.
Takes an xml document from a Hilltop MeasurementList at a Site request. 
Returns a dataframe of the datasource information and measurements names.

hilltopMeasInfoList
Helper function.
Takes an xml document from a Hilltop MeasurementList at a Site request. 
Returns a dataframe of the measurement information and datasources.

hilltopDsMeasListFull
Takes an xml document from a Hilltop MeasurementList at a Site request. 
Returns a dataframe of all of the datasource and measurement information combined.

hilltopValueHelper
Helper function to return the appropriate xml value depending whether the value of interest is from a named node, or is a named parameter.

hilltopAttributeHelper
Helper function to return the appropriate xml attribute name depending whether the attribute of interest is from a named node, or is a named parameter.

hilltopMeasurementToDF
Helper function that reads the nodes within a the Measurement node of a Hilltop XML response from a xmlParse(url) request such as dataxml<-xmlParse(url). 
Returns a dataframe of the data for each timestamp.
Handles missing results and doen't require prior knowledge of parameter names.
Handles true measurements and WQ Sample requests

hilltopDataSourceToDF
Helper function that reads the nodes within a the DataSource ItemInfo node of a Hilltop XML response from a xmlParse(url) request such as dataxml<-xmlParse(url).
Returns a dataframe of the Info for each Item.
Handles missing results and doen't require prior knowledge of the items.

hilltopMeasurement
Main function that converts a Hilltop XML document from a xmlParse(url) request such as dataxml<-xmlParse(url) for a water quality measurement into a dataframe that contains the measurement information.
It returns a dataframe of the data for each timestamp, including DataSource Information and the Site Name.
This dataframe can be merged with a WQ Sample dataframe processed using hilltopMeasurementToDF