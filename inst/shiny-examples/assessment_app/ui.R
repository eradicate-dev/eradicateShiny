#this is the app for pre-eradication assessment using occupancy, n-mixture, royle-nichols or REST models
#relies on functions in eradication package
library(shiny)
library(knitr)
library(raster)
library(sf)
library(ggspatial)
library(tidyverse)
library(R.utils)
library(kableExtra)
library(leaflet)
library(rgdal)
library(rgeos)

#read in example data to use as default inputs
#traps<-eradicate::san_nic_pre$traps
#counts<-eradicate::san_nic_pre$counts
#region<- read_sf(system.file("extdata", "shape/san_nic_region.shp", package="eradicate"))
#habitat<- raster(system.file("extdata", "san_nic_habitat.tif", package="eradicate"))

##DEFINE THE USER INTERFACE################################################################################################
ui<-fluidPage(
	tags$head(
		tags$style(HTML("hr {border-top: 1px solid #000000;}"))
	),
	titlePanel("Pre-implementation Monitoring"),
	sidebarLayout(
		sidebarPanel(#INPUT THE REGION, detector and detections FILEs ---------------------------------------------------
								 fileInput(inputId="boundary", label="Region Shapefile",
								 					multiple=TRUE,  accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
								 fileInput(inputId="habitat_raster", label="Habitat raster", accept=c(".tif", ".asc")),
								 fileInput(inputId="detectors", label="Detector locations (csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="counts", label="Detection histories (csv)", accept=c("text/csv", ".csv")),
								 hr(),
								 actionButton("Plot_design", "Plot design"),
								 hr(),
								 #SELECT APPROPRIATE MODEL --------------------------------------------------------------------------
								 radioButtons(inputId="Model", label="Choose Model", inline=TRUE,
								 						 choices=c("N-mixture"="Nmix",
								 						 					"Royle-Nichols"="RN",
								 						 					"Occupancy"="Occ",
								 						 					"REST"="REST")),
								 actionButton("Fit_mod", "Fit model")
		),
		mainPanel(
		#	tableOutput("site_bound") #for debugging, these are the shapefile components in a table
			#http://www.paulamoraga.com/book-geospatial/sec-shinyexample.html
			leafletOutput(outputId = "map", height=700)
		)
	)
)


