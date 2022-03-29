#this is the app for pre-eradication assessment using occupancy, n-mixture, royle-nichols or REST models
#relies on functions in eradication package
require(shiny)
require(shinycssloaders)
require(shinyBS)
require(waiter)
require(sf)
require(raster)
require(tidyverse)
require(R.utils)
require(eradicate)
require(leaflet)
require(rgdal)
require(rgeos)
require(xtable)
require(markdown)
##DEFINE THE USER INTERFACE################################################################################################
ui<-fluidPage(
	#splashscreen
	use_waiter(),
	waiterShowOnLoad(html = '<p col="white">Loading assessment app...</p>
									 <img src="logos.gif">', color="#666666"),
	tags$head(
		tags$style(HTML("hr {border-top: 1px solid #000000;}",
										".shiny-input-container {margin-bottom: -15px; margin-top: -15px}"))
	),
	titlePanel(title=div(
											 "Pre-eradication assessment",
											 img(src="ciss_logo.jpg", height=50, align="right"),
											 img(src="manaki_logo.png", height=50, align="right"),
											 img(src="ari_logo.jpg", height=70, align="right"))),
	sidebarLayout(
		sidebarPanel(#CHOOSE A MODEL---------------------------------- ---------------------------------------------------
								 fluidRow(column(5, radioButtons(inputId="Model", label="Select a model to use",
								 																choices=list("Occupancy"="Occ",
								 																						 "Royle-Nichols"="RN",
								 																						 "N-mixture"="Nmix",
								 																						 "REST"="REST"
								 																), selected="RN")),
								 				 conditionalPanel("input.Model!='Occ' & input.Model!='REST'",
								 				 column(3, numericInput(inputId="K", label="K", min=1, max=NA, value=50, step=1)))),
								 hr(),
								 wellPanel(
								 #THESE ARE THE INPUTS FOR EVERYTHING EXCEPT REST----------------------------------------------------
								 tipify(fileInput(inputId="boundary", label="Region boundary (.shp)",
								 					multiple=TRUE,  accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
								 			   "Component files of an ESRI shapefile for the site boundary. The shapefile should be projected, not geographic"),
								 tipify(fileInput(inputId="habitat_rasters", label="Habitat raster (.tif)",
								 					multiple=TRUE, accept=c(".tif")),
								 			   "Raster files (.tif) of habitat covariates in the same projection as the boundary.
								 			    All rasters must have the same extent and resolution"),
								 tipify(fileInput(inputId="detectors", label="Detector locations (.csv)", accept=c("text/csv", ".csv")),
								 "csv file containing the x-y coordinates of the detector devices"),
								 conditionalPanel(condition = "input.Model != 'REST'",
								 tipify(fileInput(inputId="counts", label="Detection histories or counts (.csv)", accept=c("text/csv", ".csv")),
								 "csv file containing detection histories at each detector device")
								 ),
								 #THESE ARE THE INPUTS FOR REST----------------------------------------------------------------------
								 conditionalPanel(condition = "input.Model == 'REST'",
								 tipify(fileInput(inputId="countREST", label="Detection histories or counts (.csv)", accept=c("text/csv", ".csv")),
								        "csv file containing repeated counts at each detector device"),
								 tipify(fileInput(inputId="stayREST", label="Staying and Censoring times (.csv)", accept=c("text/csv", ".csv")),
								        "csv containing staying and censoring times for REST model"),
								 fluidRow(
								 	column(3,
								 				 tipify(numericInput(inputId="activeREST", label="Active hours", value=24, min=0, max=24),
								 				 			 "Number of hours per day active")),
								 	column(4,
								 		tipify(numericInput(inputId="areaREST", label="Area of camera viewshed", value=2.67),
								 			 "Camera viewshed area. Use multiplier input to rescale units as required")),
								 column(5,
								 			 tipify(numericInput(inputId="viewshedMultiplier", label="Viewshed area multiplier", value=1000000),
								 			 			 "Muliplier to convert viewshed area units to map area units m2:ha=>10,000, m2:km2=>1,000,000"))
								 )),
								 ),
								 hr(),
								 #CONTROLS FOR THE MAP DISPLAY-----------------------------------------------------------------------
								 fluidRow(
								 column(3, actionButton("Plot_design", "Plot map")),
								 column(3, numericInput("habitat_radius", "Raster sampling radius", min=0, max=NA, value=500, step=50)),
								 column(5, sliderInput("Habitat_opacity", "Raster transparency", min=0, max=1, value=0.2, step=0.2))
								 ),
								 hr(),
								 #CONTROL TO FIT MODEL ------------------------------------------------------------------------------
								 fluidRow(
								 column(4,
								 			 tipify(checkboxGroupInput(inputId="state_formula", label="habitat covariates",
								 			 									 choices=NULL,
								 			 									 selected=NULL
								 			 									 ),"Select habitat variables for inclusion in abundance/occupancy model.
								 			 			               If none selected an intercept-only model will be fitted")

								 			 ),
								 column(3,
								 			 tipify(checkboxGroupInput(inputId="state_factors", label="habitat factor",
								 			 													choices=NULL,
								 			 													selected=NULL
								 			 ),"Select if habitat variables is a discrete factor variable")

								 ),
								 column(3, actionButton(inputId="Run_model", label="Fit model"),
								 			     actionButton(inputId="EstDens", "Estimate Density Surface"),
								 			     downloadButton(inputId="downloadraster","Download Density Raster")) ,width=7, fluid=TRUE)
								 ),

		mainPanel(
			tabsetPanel(id="maintabs", type="tabs",
									#Map tab
									tabPanel(title="Map",	                value="panel1",
													    leafletOutput(outputId = "map", height=700)),
									#Fitted models tab
									tabPanel(title="Fitted model",        value="panel2",
													    fluidRow( tableOutput(outputId="parameter_table") %>% withSpinner(type=4)),
													    fluidRow( tableOutput(outputId="abundance_table") ),
													    fluidRow( textOutput(outputId="AIC") ),
													 ),
									tabPanel(title="Help", value="panel3", includeMarkdown("assessment_help.md")))
		)
	),
	bsTooltip("K", 'K parameter for Royle-Nichols and N-mixture models. The app calculates a sensible default. Large values will increase computation time significantly.')
) #end UI


