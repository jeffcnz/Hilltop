# Hilltop
R functions for interacting with a Hilltop Time Series Server

The file Hilltop.R contains functions for interacting with a Hilltop server.

The file WQualityImport_Example.R demonstrates the use of some of the Hilltop.R functions for downloading water quality data from a Hilltop server. 

Hilltop.R

Functions

hillSiteList
Work in progress - Given a server endpoint, requests and parses a SiteList request and generates a dataframe of sites.

dataSourcesAtSite
Work in progress - Given a parsed xml MeasurementList request creates a data frame of the data source information.  Not complete.

measAtSite
Given a server endpoint and site name, requests and parses a MeasurementList request for the site and creates a data frame of the data source information.  Not complete.

xmlHilltopValueHelper
Helper function to return the appropriate xml value depending whether the value of interest is from a named node, or is a named parameter.

xmlHilltopAttributeHelper
Helper function to return the appropriate xml attribute name depending whether the attribute of interest is from a named node, or is a named parameter.

xmlHilltopMeasurementToDF
Helper function that reads the nodes within a the Measurement node of a Hilltop XML response from a xmlParse(url) request such as dataxml<-xmlParse(url). 
Returns a dataframe of the data for each timestamp.
Handles missing results and doen't require prior knowledge of parameter names.
Handles true measurements and WQ Sample requests

xmlHilltopDataSourceToDF
Helper function that reads the nodes within a the DataSource ItemInfo node of a Hilltop XML response from a xmlParse(url) request such as dataxml<-xmlParse(url).
Returns a dataframe of the Info for each Item.
Handles missing results and doen't require prior knowledge of the items.

xmlHilltopMeasurement
Main function that converts a Hilltop XML document from a xmlParse(url) request such as dataxml<-xmlParse(url) for a water quality measurement into a dataframe that contains the measurement information.
It returns a dataframe of the data for each timestamp, including DataSource Information and the Site Name.
This dataframe can be merged with a WQ Sample dataframe processed using xmlHilltopMeasurementToDF