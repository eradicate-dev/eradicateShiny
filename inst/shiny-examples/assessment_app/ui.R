#this is the app for pre-eradication assessment using occupancy, n-mixture, royle-nichols or REST models
#relies on functions in eradication package
library(shiny)
library(shinycssloaders)
library(raster)
library(sf)
library(tidyverse)
library(R.utils)
library(leaflet)
library(rgdal)
library(rgeos)
library(eradicate)
library(xtable)

##DEFINE THE USER INTERFACE################################################################################################
ui<-fluidPage(
	tags$head(
		tags$style(HTML("hr {border-top: 1px solid #000000;}",
										".shiny-input-container {margin-bottom: -10px;}"))
	),
	titlePanel("Pre-implementation Monitoring"),
	sidebarLayout(
		sidebarPanel(#INPUT THE REGION, detector and detections FILEs ---------------------------------------------------
								 fileInput(inputId="boundary", label="Region boundary (.shp)",
								 					multiple=TRUE,  accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
								 fileInput(inputId="habitat_rasters", label="Habitat raster (.tif, .asc)", accept=c(".tif", ".asc")),
								 fileInput(inputId="detectors", label="Detector locations (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="counts", label="Detection histories or counts (.csv)", accept=c("text/csv", ".csv")),
								 hr(),
								 fluidRow(
								 column(3, actionButton("Plot_design", "Plot map")),
								 column(3, numericInput("habitat_radius", "Raster sampling radius", min=0, max=NA, value=500, step=50)),
								 column(5, sliderInput("Habitat_opacity", "Raster opacity", min=0, max=1, value=0.2, step=0.2))
								 ),
								 hr(),
								 #SELECT APPROPRIATE MODEL --------------------------------------------------------------------------
								 fluidRow(
								 column(3, radioButtons(inputId="Model", label="Select model",
								 						 choices=list("Occupancy"="Occ",
								 						 						 "Royle-Nichols"="RN",
								 						 	            "N-mixture"="Nmix"
								 						 					    ), selected="RN")),
								 conditionalPanel("input.Model!='Occ'",
								 column(2, numericInput(inputId="K", label="K", min=1, max=NA, value=50, step=1),
								           checkboxInput("EstDens", "Estimate Density", value=FALSE))),
								 column(2, actionButton(inputId="Run_model", label="Fit model") ) ,width=4, fluid=TRUE)
								 ),

		mainPanel(
			tabsetPanel(id="maintabs", type="tabs",
									#Map tab
									tabPanel(title="Map",	                value="panel1",
													    leafletOutput(outputId = "map", height=700)),
									#Fitted models tab
									tabPanel(title="Fitted model",        value="panel2",
													    fluidRow( tableOutput(outputId="parameter_table") %>% withSpinner(type=4)),
													    fluidRow( tableOutput(outputId="abundance_table") %>% withSpinner(type=4)),
													 ))
		)
	)
)


