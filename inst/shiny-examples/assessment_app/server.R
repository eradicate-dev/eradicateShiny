##DEFINE THE SERVER#####################################################################################################
server<-function(input, output){
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
		{traps<-eradicate::san_nic_pre$counts} else
		{traps<-read_csv(input$counts$datapath) }
		counts
	})



#Render a map of the outputs
output$map<-renderLeaflet({
	req(input$boundary)
	req(input$Plot_design)
	#set up and tranform site boundary
	bound<-site_bound()
	detectors<-st_as_sf(detectors(), coords = c("x", "y"), crs=st_crs(bound))
#	habras<-hab_raster()
#	crs(habras)<-crs(bound) #assume same crs as region boundary
#	habras2<-projectRaster(habras, to="+init=epsg:4326")
	leaflet() %>%
		addProviderTiles("Esri.WorldTopoMap") %>%
 # 	addRasterImage(habras2) %>%
		addPolygons(data=st_transform(bound, "+init=epsg:4326")) %>%
		addCircleMarkers(data=st_transform(detectors, "+init=epsg:4326"), color="red", radius=2)
})

}
