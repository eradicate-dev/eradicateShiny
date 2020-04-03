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
	titlePanel("Eradication Monitoring"),
	sidebarLayout(
		sidebarPanel(#INPUT THE REGION, detector and detections FILEs ---------------------------------------------------
								 fileInput(inputId="boundary", label="Region Boundary (.shp)",
								 					multiple=TRUE,  accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
								 fileInput(inputId="habitat_rasters", label="Habitat raster (.tif, .asc)", accept=c(".tif", ".asc")),
								 fileInput(inputId="traps", label="Trap locations (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="removals", label="Removal histories (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="mtraps", label="Monitored traps (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="detections", label="Detection histories (.csv)", accept=c("text/csv", ".csv")),
								 numericInput("nights", "Nights per session", min=0, max=NA, value=10, step=1),
								 hr(),
								 fluidRow(
								 	column(3, actionButton("Plot_design", "Plot design")),
								 	column(2, actionButton("Plot_removal", "Plot removal progress"))
								 ),
								 sliderInput("Habitat_opacity", "Habitat opacity", min=0, max=1, value=0.2, step=0.1),
								 numericInput("habitat_radius", "Habitat sampling radius", min=0, max=NA, value=500, step=50),
								 hr(),
								 #SELECT APPROPRIATE MODEL --------------------------------------------------------------------------
								 fluidRow(
								 radioButtons(inputId="Model", label="Model to Fit",
								 						 choices=list("remPois"="remPois",
								 						 						 "remGR"="remGR",
								 						 						 "remGRM"="remGRM",
								 						 						 "remGP (aspatial)"="remGP"
								 						 					    ), selected="remPois"),

								 ),
								 actionButton(inputId="Run_model", label="Fit Model")
		,width=4),
		mainPanel(
			tabsetPanel(id="maintabs", type="tabs",
									#Map tab
									tabPanel(title="Map",	                value="panel1",
													    leafletOutput(outputId = "map", height=700)),
									tabPanel(title="Removals", value="panel3",
													 fluidRow( plotOutput(outputId="removal_plot")),
													 fluidRow( plotOutput(outputId="detection_plot"))),
									#Fitted models tab
									tabPanel(title="Fitted Model",        value="panel2",
													    fluidRow( tableOutput(outputId="parameter_table") %>% withSpinner(type=4)),
													    fluidRow( tableOutput(outputId="abundance_table") %>% withSpinner(type=4))
													 ))
		)
	)
)


