##DEFINE THE SERVER#####################################################################################################
server<-function(input, output, session){
#Boundary of study area, in a shapefile
	site_bound<-reactive({
		myshape<-input$boundary
		dir<-dirname(myshape[1,4])
		for ( i in 1:nrow(myshape)) {
			file.rename(myshape[i,4], paste0(dir,"/",myshape[i,1]))
		}
		getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
		st_read(getshp, quiet=TRUE)
	})
#raster of habitat suitability. default is San Nicolas
hab_raster<-reactive({
		if(is.null(input$habitat_raster))
			{habras<-raster(system.file("extdata", "san_nic_habitat.tif", package="eradicate"))} else
		  {habras<-raster(input$habitat_raster$datapath) }
		habras
	})
#csv of detectors. default is San Nicolas
detectors<-reactive({
		if(is.null(input$detectors))
			{traps<-eradicate::san_nic_pre$traps} else
			{traps<-read_csv(input$detectors$datapath) }
		traps
	})

#counts
counts<-reactive({
		if(is.null(input$counts))
		{counts<-eradicate::san_nic_pre$counts} else
		{counts<-read_csv(input$counts$datapath) }
		counts
	})

#habitat radius user input
buff<-reactive({
 input$habitat_radius
})

habopacity<-reactive({
	input$Habitat_opacity
})

###################################################################################################
#reactive function for habitat value extraction from raster
###################################################################################################
habmean<-reactive({
	rast<-hab_raster()
	buff<-buff()
	dets<-detectors()
	habvals<-raster::extract(rast, dets, buffer=buff)
	habmean<- sapply(habvals, function(x) mean(x, na.rm=T))
	habmean
})


#############################################################
#  -- respond to UI events
#############################################################
#jump focus to appropriate tab on button press
observeEvent(input$Run_model, {
	updateTabsetPanel(session, "maintabs",
										selected = "panel2")
})
observeEvent(input$Plot_design, {
	updateTabsetPanel(session, "maintabs",
										selected = "panel1")
})
observeEvent(input$EstDens, {
	updateTabsetPanel(session, "maintabs",
										selected = "panel1")
})
#disable density if using occupancy model
observe(if(input$Model!="Occ") {
	updateNumericInput(session, "K", value=5*max(counts()) + 50) #sets default value for K
})

#############################################################
#  -- Model fitting
#############################################################
ModToFit<-reactive({
	input$Model
})
K<-reactive({
	input$K
})
EstDens<-reactive({
	input$EstDens
})

#fit the selected model to the data.
fit_mod<-reactive({
dets<- detectors()
cnts<-counts()
modname<-ModToFit()
K<-K()
habmean<-habmean()
site.data<- cbind(dets, habmean)
emf<-   eradicate::eFrame(cnts, siteCovs = site.data)
if(modname== "Occ" ) {model<-eradicate::occuM(~habmean, ~1, data=emf)} else
if(modname== "RN"  ) {model<-eradicate::occuRN(~habmean, ~1, K=K, data=emf)} else
if(modname== "Nmix") {model<-eradicate::nmix(~habmean, ~1, K=K, data=emf)}
model
})

#summary table of parameter estimates for selected model
summary_tab<-reactive({
mod<-fit_mod()
out<-summary(mod)
npar1<-nrow(out[[1]])
npar2<-nrow(out[[2]])
out<-rbind(out[[1]], out[[2]])
data.frame(out)
out
})

#summary table of abundance estimates conditional on selected model
abund_tab<-reactive({
	modname<-ModToFit()
	mod<-fit_mod()
	buff<-buff()  #buffer zone radius
	#If occupancy, no buffer offset applies, else offset estimate with buffer area
	if(modname=="Occ") {Nhat<-calcN(mod)} else {Nhat<-calcN(mod)}
  if(modname=="Occ"){out<-Nhat$Occ} else
  if(modname=="RN"){out<-Nhat$Nhat} else
  if(modname=="Nmix"){out<-Nhat$Nhat}
	out
})

DensRast<-reactive({
	modname<-ModToFit()
	mod<-fit_mod()
	buff<-buff()  #buffer zone radius
	rast<-hab_raster()
	bound<-site_bound()
#code in here to calculate density/occ surface
	filt<-focalWeight(x=rast, d=buff, type='circle')
	rast2<-focal(x=rast, w=filt,fun=sum, na.rm=TRUE, pad=TRUE)
	vals<-getValues(rast2)
	coeffs<-mod$state$estimates
	preds.lin<-vals*coeffs[2] + coeffs[1]
	if(modname=="Occ"){preds<-plogis(preds.lin)} else
	                  {preds<-exp(preds.lin)}
	predras<-raster(rast)
	predras[]<-preds
	predras<-mask(predras, bound)
	predras
})

###########################################################################################
#
#   --- RENDER THE OUTPUTS  ----
#
###########################################################################################

#render a table of parameter estimates
output$parameter_table<-renderTable({
req(input$Run_model)
  summary_tab()
}, row.names=FALSE, width=300, caption="Parameter estimates", caption.placement = "top")

#render table of abundance estimates
output$abundance_table<-renderTable({
	req(input$Run_model)
	abund_tab()
}, row.names=FALSE, width=300,
   caption="Real estimates",
caption.placement = getOption("xtable.caption.placement", "top"),
caption.width = getOption("xtable.caption.width", NULL))

#download the density raster
output$downloadraster <- downloadHandler(
	filename = "density_raster.tif",
	content = function(file) {
		writeRaster(DensRast(), file, format="GTiff", overwrite=TRUE)
	}, contentType = "image/tif"
)

#Render a map of the input data
output$map<-renderLeaflet({
	req(input$boundary)
	req(input$Plot_design)
	req(input$Habitat_opacity)
	req(input$habitat_radius)
	#set up and tranform site boundary
	bound<-site_bound()
	detectors<-st_as_sf(detectors(), coords = c("x", "y"), crs=st_crs(bound))
	detector_buff<-st_buffer(detectors, dist=buff())
	habras<-hab_raster()
  crs(habras)<-crs(bound) #assume same crs as region boundary
	pal <- colorNumeric("viridis", values(habras), na.color = "transparent")
	leaflet() %>%
		addTiles(group="OSM") %>%
		addProviderTiles("Esri.WorldTopoMap", group="ESRI Topo") %>%
		addProviderTiles("Esri.WorldImagery", group="ESRI Satellite") %>%
  	addRasterImage(projectRaster(habras, crs="+init=epsg:4326", method="ngb"), colors=pal,
  								 opacity=habopacity(), group="habitat raster") %>%
		addPolygons(data=st_transform(bound, "+init=epsg:4326"), weight=2, fill=FALSE) %>%
		addCircleMarkers(data=st_transform(detectors, "+init=epsg:4326"), color="red", radius=1, group="detectors") %>%
		addPolygons(data=st_transform(detector_buff, "+init=epsg:4326"), color="red", weight=1, group="detector buffer") %>%
		addScaleBar("bottomleft", options=scaleBarOptions(imperial=FALSE, maxWidth = 200)) %>%
		addLayersControl(baseGroups=c("OSM (default)", "ESRI Topo", "ESRI Satellite"),
										 overlayGroups = c("detectors", "detector buffer", "habitat raster"),
										 options=layersControlOptions(collapsed=FALSE))
}
)

observe({
	req(input$EstDens)
	req(input$boundary)
	req(input$Habitat_opacity)
	req(input$Run_model)
	bound<-site_bound()
	modname<-ModToFit()
	if(modname=="Occ"){leglab<-"Pr(Occ)"} else {leglab<-"Density"}
	DensRast <- DensRast()
	crs(DensRast)<-crs(bound)
	pal <- colorNumeric("viridis", values(DensRast), na.color = "transparent")
	leafletProxy("map") %>%
		clearImages() %>%
		clearControls() %>%
		addRasterImage(projectRaster(DensRast, crs="+init=epsg:4326", method="ngb"), colors=pal,
							opacity=habopacity(), group="density raster") %>%
		addLegend(position="bottomright", pal=pal, bins=6, title=leglab, values=values(DensRast)) %>%
		addLayersControl(baseGroups=c("OSM (default)", "ESRI Topo", "ESRI Satellite"),
										 overlayGroups = c("detectors", "detector buffer", "habitat raster", "density raster"),
										 options=layersControlOptions(collapsed=FALSE)) %>%
		hideGroup(c("habitat raster","detector buffer", "detectors"))
})



}

