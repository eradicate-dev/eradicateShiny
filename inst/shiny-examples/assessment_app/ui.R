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
								 fileInput(inputId="boundary", label="Region Boundary (.shp)",
								 					multiple=TRUE,  accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
								 fileInput(inputId="habitat_rasters", label="Habitat raster (.tif, .asc)", accept=c(".tif", ".asc")),
								 fileInput(inputId="detectors", label="Detector locations (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="counts", label="Detection histories (.csv)", accept=c("text/csv", ".csv")),
								 hr(),
								 actionButton("Plot_design", "Plot design"),
								 sliderInput("Habitat_opacity", "Habitat opacity", min=0, max=1, value=0.5, step=0.1),
								 numericInput("habitat_radius", "Habitat sampling radius", min=0, max=NA, value=500, step=50),
								 hr(),
								 #SELECT APPROPRIATE MODEL --------------------------------------------------------------------------
								 radioButtons(inputId="Model", label="Model to Fit",
								 						 choices=list("Occupancy"="Occ",
								 						 						 "Royle-Nichols"="RN",
								 						 	            "N-mixture"="Nmix"
								 						 					    ), selected="Occ"),
								 numericInput("K", "K", min=1, max=NA, value=50, step=1),
								 actionButton(inputId="Run_model", label="Run Model")
		),
		mainPanel(
			tabsetPanel(id="maintabs", type="tabs",
									#Map tab
									tabPanel(title="Map",	                value="panel1",
													    leafletOutput(outputId = "map", height=700)),
									#Fitted models tab
									tabPanel(title="Fitted Model",        value="panel2",
													    fluidRow( tableOutput(outputId="parameter_table") %>% withSpinner(type=4)),
													    fluidRow( tableOutput(outputId="abundance_table") %>% withSpinner(type=4)),
													 ))
		)
	)
)


