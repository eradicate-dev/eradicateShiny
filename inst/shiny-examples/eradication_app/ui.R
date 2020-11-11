#this is the app for monitoring eradication assessment using a variety of removal models
#relies on the functions in the eradicate package
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
##DEFINE THE USER INTERFACE################################################################################################
ui<-fluidPage(
	#splashscreen
	use_waiter(),
	waiter_show_on_load(html = '<p col="white">Loading eradication app...</p>', logo="logos.gif", color="#666666"),
	tags$head(
		tags$style(HTML("hr {border-top: 1px solid #000000;}",
										".shiny-input-container {margin-bottom: -15px; margin-top: -15px}"))
	),
	titlePanel(title=div(
		"Eradication monitoring",
		img(src="ari_logo.jpg", height=50, align="right"),
		img(src="ciss_logo.jpg", height=50, align="right"),
		img(src="manaki_logo.png", height=50, align="right"))),
	sidebarLayout(
		sidebarPanel(#INPUT THE REGION, detector and detections FILEs ---------------------------------------------------
								 fluidRow(
								 	column(7,
								 				 tipify(radioButtons(inputId="Model", label="Select model",
								 				 						 choices=list("remGP (aspatial)"="remGP", #have remove remCE and remGR
								 				 						 						 "remMN"="remMN",  #equivalent to remGR
								 				 						 						 "remGRM"="remGRM"#,
								 				 						 					#	 "remMNO"="remMNO", #hidden remMNO and occuMS for now
								 				 						 					#	 "occuMS"="occuMS"
								 				 						 ), selected="remMN"), "remGP-aspatial removal data<br>remMN-spatially referenced removal data<br>remGRM-spatially referenced removal data with auxilary detections")
								 				 ),
								 	#conditionally take input for parameter K, depending on model type.
								 	conditionalPanel("input.Model!='remCE'&&input.Model!='remMN'&&input.Model!='occuMS'",
								 									 column(2,
								 									 			 tipify(
								 									 			 numericInput(inputId="K", label="K", min=1, max=NA, value=50, step=1), "upper bound for integration of abundance")
								 									 ))),
								 hr(),
								 wellPanel(
								 	#hide spatial input tools if using an aspatial model
								 	conditionalPanel("input.Model!='remCE'&&input.Model!='remGP'",
								 fileInput(inputId="boundary", label="Region boundary (.shp)",
								 					multiple=TRUE,  accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
								 tipify(fileInput(inputId="habitat_rasters", label="Habitat raster (.tif)",
								 								 multiple=TRUE, accept=c(".tif")),
								 			 "Raster files (.tif) of habitat covariates in the same projection as the boundary.
								 			    All rasters must have the same extent and resolution"),
								 fileInput(inputId="traps", label="Trap locations (.csv)", accept=c("text/csv", ".csv"))), #end conditional block
								 fileInput(inputId="removals", label="Removal histories (.csv)", accept=c("text/csv", ".csv")),
								 conditionalPanel("input.Model=='remGRM'",
								 fileInput(inputId="detections", label="Detection histories (.csv)", accept=c("text/csv", ".csv"))), #end conditional block?
								 fluidRow(
								 	column(4,
								 conditionalPanel("input.Model=='remMNO' | input.Model=='occuMS'",
								 numericInput("nights", "Nights per primary session", min=0, max=NA, value=10, step=1))))
								 ),
								 hr(),
								 fluidRow(
								 	column(3,
								 				 conditionalPanel("input.Model!='remCE'&&input.Model!='remGP'",
								 				 actionButton("Plot_design", "Plot map")), #end conditional
								 				    actionButton("Plot_removal", "Plot removals")),
								 	conditionalPanel("input.Model!='remCE'&&input.Model!='remGP'",
								 	column(4, numericInput("habitat_radius", "Raster sampling radius", min=0, max=NA, value=500, step=50)),
								 	column(5, sliderInput("Habitat_opacity", "Raster opacity", min=0, max=1, value=0.2, step=0.2)))
								 ),
								 hr(),
								 #SELECT APPROPRIATE MODEL --------------------------------------------------------------------------
								 fluidRow(
								 	column(6,conditionalPanel("input.Model!='remCE'&&input.Model!='remGP'",
								 				 tipify(checkboxGroupInput(inputId="state_formula", label="habitat covariates",
								 				 													choices=NULL,
								 				 													selected=NULL
								 				 ),"Select habitat variables for inclusion in abundance model.
								 			 			               If none selected an intercept-only model will be fitted"))

								 	),
								 	column(3, actionButton(inputId="Run_model", label="Fit model"),
								 				 actionButton(inputId="EstDens", "Estimate Density Surface"),
								 				 downloadButton(inputId="downloadraster","Download Density Raster")) ,width=7, fluid=TRUE)),
		mainPanel(
			tabsetPanel(id="maintabs", type="tabs",
									#Map tab
									tabPanel(title="Map",	                value="panel1",
													    leafletOutput(outputId = "map", height=700)),
									tabPanel(title="Removals", value="panel3",
													 fluidRow( plotOutput(outputId="removal_plot", width="60%")),
													 conditionalPanel("input.Model=='remGRM'",
													 fluidRow( plotOutput(outputId="detection_plot", width="60%"))) #end conditional panel
													 ),
									#Fitted models tab
									tabPanel(title="Fitted Model",        value="panel2",
													    fluidRow( tableOutput(outputId="parameter_table") %>% withSpinner(type=4)),
													    fluidRow( tableOutput(outputId="abundance_table")),
													    fluidRow( textOutput(outputId="AIC") ),
													 ))
		)
	)
)


