#this is the app for pre-eradication assessment using occupancy, n-mixture, royle-nichols or REST models
#relies on functions in eradication package
library(shiny)
library(shinycssloaders)
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
library(eradicate)

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
								 fileInput(inputId="habitat_rasters", label="Habitat raster (.tif, .asc)", accept=c(".tif", ".asc")),
								 fileInput(inputId="detectors", label="Detector locations (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="counts", label="Detection histories (.csv)", accept=c("text/csv", ".csv")),
								 hr(),
								 actionButton("Plot_design", "Plot design"),
								 hr(),
								 #SELECT APPROPRIATE MODEL --------------------------------------------------------------------------
								 checkboxGroupInput(inputId="Models", label="Models to Fit",
								 						 choices=list("N-mixture"="Nmix",
								 						 					    "Royle-Nichols"="RN",
								 						 					    "Occupancy"="Occ",
								 						 					    "REST"="REST")),
								 sliderInput("habitat_radius", "Habitat radius", min=0, max=5000, value=500, step=500),
								 actionButton(inputId="Run_model", label="Run Model (s)")
		),
		mainPanel(
			tabsetPanel(id="maintabs", type="tabs",
									tabPanel(title="Map",	                value="panel1",
													    leafletOutput(outputId = "map", height=700)),
									tabPanel(title="Fitted Model",        value="panel2",
													    textOutput(outputId="model") %>% withSpinner(type=4)),
			            tabPanel(title="Abundance estimates", value="panel3",
			            				 textOutput(outputId="abundance") %>% withSpinner(type=4) ) )
		)
	)
)


