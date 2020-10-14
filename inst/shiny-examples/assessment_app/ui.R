#this is the app for pre-eradication assessment using occupancy, n-mixture, royle-nichols or REST models
#relies on functions in eradication package
##DEFINE THE USER INTERFACE################################################################################################
ui<-fluidPage(
	#splashscreen
	use_waiter(),
	waiter_show_on_load(html = '<p col="white">Loading eradicate...</p>', logo="logos.gif", color="#666666"),
	tags$head(
		tags$style(HTML("hr {border-top: 1px solid #000000;}",
										".shiny-input-container {margin-bottom: -15px; margin-top: -15px}"))
	),
	titlePanel(title=div(
											 "eradicate:  pre-eradication assessment",
											 img(src="ari_logo.jpg", height=50, align="right"),
											 img(src="ciss_logo.jpg", height=50, align="right"),
											 img(src="manaki_logo.png", height=50, align="right"))),
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
								 tipify(fileInput(inputId="habitat_rasters", label="Habitat raster (.tif, .asc)",
								 					multiple=TRUE, accept=c(".tif", ".asc")),
								 			   "Raster files (.tif or .asc) of habitat covariates in the same projection as the boundary.
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
								 tipify(fileInput(inputId="stayREST", label="Stay (.csv)", accept=c("text/csv", ".csv")),
								        "csv containing staying times for REST model"),
								 tipify(fileInput(inputId="censREST", label="Censored (.csv)", accept=c("text/csv", ".csv")),
								       "csv containing censoring status for observations (REST model)"),
								 fluidRow(
								 	column(3,
								 				 tipify(numericInput(inputId="activeREST", label="Active hours", value=24, min=0, max=24),
								 				 			 "Number of hours per day active")),
								 	column(4,
								 		tipify(numericInput(inputId="areaREST", label="Area of camera viewshed", value=2.67),
								 			 "Camera viewshed area (in km^2) - use multiplier argument to rescale as required")),
								 column(5,
								 			selectInput(inputId="viewshedMultiplier", label="Viewshed area multiplier",
								 									choices=c("1"=1,"10^-3"=10e-3, "10^-6"=10e-6, "10^-9"=10e-9), selected=10e-6))
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
								 column(3, actionButton(inputId="Run_model", label="Fit model"),
								 			     actionButton("EstDens", "Estimate Density Surface"),
								 			     downloadButton("downloadraster", "Download Density Raster")) ,width=4, fluid=TRUE)
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
	),
	bsTooltip("K", 'K parameter for Royle-Nichols and N-mixture models. The app calculates a sensible default. Large values will increase computation time significantly.')
) #end UI


