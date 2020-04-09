#this is the app for monitoring eradication assessment using a variety of removal models
#relies on the functions in the eradicate package
##DEFINE THE USER INTERFACE################################################################################################
ui<-fluidPage(
	tags$head(
		tags$style(HTML("hr {border-top: 1px solid #000000;}",
										".shiny-input-container {margin-bottom: -10px;}"))
	),
	titlePanel("Eradication Monitoring"),
	sidebarLayout(
		sidebarPanel(#INPUT THE REGION, detector and detections FILEs ---------------------------------------------------
								 fileInput(inputId="boundary", label="Region boundary (.shp)",
								 					multiple=TRUE,  accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
								 fileInput(inputId="habitat_rasters", label="Habitat raster (.tif, .asc)", accept=c(".tif", ".asc")),
								 fileInput(inputId="traps", label="Trap locations (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="removals", label="Removal histories (.csv)", accept=c("text/csv", ".csv")),
								 fileInput(inputId="detections", label="Detection histories (.csv)", accept=c("text/csv", ".csv")),
								 numericInput("nights", "Nights per primary session", min=0, max=NA, value=10, step=1),
								 hr(),
								 fluidRow(
								 	column(3, actionButton("Plot_design", "Plot map"), actionButton("Plot_removal", "Plot removals")),
								 	column(4, numericInput("habitat_radius", "Raster sampling radius", min=0, max=NA, value=500, step=50)),
								 	column(5, sliderInput("Habitat_opacity", "Raster opacity", min=0, max=1, value=0.2, step=0.2))
								 ),
								 hr(),
								 #SELECT APPROPRIATE MODEL --------------------------------------------------------------------------
								 fluidRow(
								 column(3,
								 radioButtons(inputId="Model", label="Select model",
								 						 choices=list("remPois"="remPois",
								 						 						 "remGR"="remGR",
								 						 						 "remGRM"="remGRM",
								 						 						 "remGP (aspatial)"="remGP"
								 						 					    ), selected="remPois")),
								 #conditionally take input for parameter K, depending on model type.
								 conditionalPanel("input.Model!='remPois'&&input.Model!='remGP'",
								 								 column(2, numericInput(inputId="K", label="K", min=1, max=NA, value=50, step=1)
								 								 			 )),
								 column(2, actionButton(inputId="Run_model", label="Fit model")))

		,width=4),
		mainPanel(
			tabsetPanel(id="maintabs", type="tabs",
									#Map tab
									tabPanel(title="Map",	                value="panel1",
													    leafletOutput(outputId = "map", height=700)),
									tabPanel(title="Removals", value="panel3",
													 fluidRow( plotOutput(outputId="removal_plot", width="60%")),
													 fluidRow( plotOutput(outputId="detection_plot", width="60%"))),
									#Fitted models tab
									tabPanel(title="Fitted Model",        value="panel2",
													    fluidRow( tableOutput(outputId="parameter_table") %>% withSpinner(type=4)),
													    fluidRow( tableOutput(outputId="abundance_table") %>% withSpinner(type=4))
													 ))
		)
	)
)


