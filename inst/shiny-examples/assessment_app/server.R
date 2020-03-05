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

#jump to the model results tab after pushing model fit button
observeEvent(input$Run_model, {
	updateTabsetPanel(session, "maintabs",
										selected = "panel2")
})


#extract habitat data from rasters
fitted_mod<-reactive({
dets<- detectors()
rast<-hab_raster()
cnts<-counts()
buff<-buff()
habvals<-raster::extract(rast, dets, buffer=buff)
habmean<- sapply(habvals, function(x) mean(x, na.rm=T))
site.data<- cbind(dets, habmean)
emf<- eradicate::eFrame(cnts, siteCovs = site.data)
mOccu<- eradicate::occuM(~habmean, ~1, data=emf)
out<-summary(mOccu)
out
})

#display summary of fitted model
output$model<-renderText({
req(input$Run_model)
paste(fitted_mod())
})

#Render a map of the input data
output$map<-renderLeaflet({
	req(input$boundary)
	req(input$Plot_design)
	#set up and tranform site boundary
	bound<-site_bound()
	detectors<-st_as_sf(detectors(), coords = c("x", "y"), crs=st_crs(bound))
	habras<-hab_raster()
	crs(habras)<-crs(bound) #assume same crs as region boundary
	pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(habras),
											na.color = "transparent")
	leaflet() %>%
		addTiles(group="OSM") %>%
		addProviderTiles("Esri.WorldTopoMap", group="ESRI Topo") %>%
		addProviderTiles("Esri.WorldImagery", group="ESRI Satellite") %>%
		addGraticule(interval=0.5) %>%
  	addRasterImage(projectRaster(habras, crs="+init=epsg:4326", method="ngb"), colors=pal, opacity=0.5, group="habitat") %>%
		addPolygons(data=st_transform(bound, "+init=epsg:4326"), weight=2, fill=FALSE) %>%
		addCircleMarkers(data=st_transform(detectors, "+init=epsg:4326"), color="red", radius=2, group="detectors") %>%
		addScaleBar("bottomleft") %>%
		addLayersControl(baseGroups=c("OSM (default)", "ESRI Topo", "ESRI Satellite"),
										 overlayGroups = c("detectors", "habitat"))
})

}

