#this is the app for pre-eradication assessment using occupancy, n-mixture, royle-nichols or REST models
#relies on functions in eradication package
##DEFINE THE USER INTERFACE################################################################################################
ui<-fluidPage(
	tags$head(
		tags$style(HTML("hr {border-top: 1px solid #000000;}",
										".shiny-input-container {margin-bottom: -10px;}"))
	),
	titlePanel("Pre-implementation Monitoring"),
	sidebarLayout(
		sidebarPanel(#CHOOSE A MODEL---------------------------------- ---------------------------------------------------
								 fluidRow(column(5, radioButtons(inputId="Model", label="Select a model to use",
								 																choices=list("Occupancy"="Occ",
								 																						 "Royle-Nichols"="RN",
								 																						 "N-mixture"="Nmix",
								 																						 "REST"="REST"
								 																), selected="RN"))),
								 hr(),
								 #THESE ARE THE INPUTS FOR EVERYTHING EXCEPT REST----------------------------------------------------
								 fileInput(inputId="boundary", label="Region boundary (.shp)",
								 					multiple=TRUE,  accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
								 fileInput(inputId="habitat_rasters", label="Habitat raster (.tif, .asc)", accept=c(".tif", ".asc")),
								 fileInput(inputId="detectors", label="Detector locations (.csv)", accept=c("text/csv", ".csv")),
								 conditionalPanel(condition = "input.Model != 'REST'",
								 fileInput(inputId="counts", label="Detection histories or counts (.csv)", accept=c("text/csv", ".csv"))
								 ),
								 #THESE ARE THE INPUTS FOR REST----------------------------------------------------------------------
								 conditionalPanel(condition = "input.Model == 'REST'",
								 fileInput(inputId="countREST", label="Detection histories or counts (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="stayREST", label="Stay (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="censREST", label="Censored (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="activeREST", label="Active (.csv)", accept=c("text/csv", ".csv")),
								 numericInput(inputId="areaREST", label="Area of camera viewshed", value=2.67e-06, min=0)
								 ),
								 hr(),
								 #CONTROLS FOR THE MAP DISPLAY-----------------------------------------------------------------------
								 fluidRow(
								 column(3, actionButton("Plot_design", "Plot map")),
								 column(3, numericInput("habitat_radius", "Raster sampling radius", min=0, max=NA, value=500, step=50)),
								 column(5, sliderInput("Habitat_opacity", "Raster opacity", min=0, max=1, value=0.2, step=0.2))
								 ),
								 hr(),
								 #CONTROL TO FIT MODEL ------------------------------------------------------------------------------
								 fluidRow(
								 conditionalPanel("input.Model!='Occ'",
								 column(2, numericInput(inputId="K", label="K", min=1, max=NA, value=50, step=1)),

								 column(2, actionButton(inputId="Run_model", label="Fit model"),
								 			     actionButton("EstDens", "Estimate Density Surface"),
								 			     downloadButton("downloadraster", "Download Density Raster")) ,width=4, fluid=TRUE))
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


