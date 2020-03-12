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

#jump focus to appropriate tab on button press
observeEvent(input$Run_model, {
	updateTabsetPanel(session, "maintabs",
										selected = "panel2")
})
observeEvent(input$Plot_design, {
	updateTabsetPanel(session, "maintabs",
										selected = "panel1")
})


#which  models to fit?
ModToFit<-reactive({
	input$Model
})
K<-reactive({
	input$K
})

#fit the selected model to the data.
fit_mod<-reactive({
dets<- detectors()
rast<-hab_raster()
cnts<-counts()
buff<-buff()
modname<-ModToFit()
K<-K()
print(modname)
#prep the data
habvals<-raster::extract(rast, dets, buffer=buff)
habmean<- sapply(habvals, function(x) mean(x, na.rm=T))
site.data<- cbind(dets, habmean)
emf<-   eradicate::eFrame(cnts, siteCovs = site.data)
if(modname== "Occ" ) {model<-eradicate::occuM(~habmean, ~1, data=emf)} else
if(modname== "RN"  ) {model<-eradicate::occuRN(~habmean, ~1, K=K, data=emf)} else
if(modname== "Nmix") {model<-eradicate::nmix(~habmean, ~1, K=K, data=emf)}
model
})

#summary table of parameter estimates
summary_tab<-reactive({
mod<-fit_mod()
out<-summary(mod)
npar1<-nrow(out[[1]])
npar2<-nrow(out[[2]])
out<-rbind(out[[1]], out[[2]])
data.frame(out)
out
})

#summary table of abundance estimates
abund_tab<-reactive({
	modname<-ModToFit()
	mod<-fit_mod()
	Nhat<-calcN(mod)
  if(modname=="Occ"){out<-Nhat$Occ} else
  if(modname=="RN"){out<-Nhat$Nhat} else
  if(modname=="Nmix"){out<-Nhat$Nhat}
	out
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
}, row.names=FALSE, width=300, caption="Abundance estimate", caption.placement = "top")

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
	pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(habras),
											na.color = "transparent")
	leaflet() %>%
		addTiles(group="OSM") %>%
		addProviderTiles("Esri.WorldTopoMap", group="ESRI Topo") %>%
		addProviderTiles("Esri.WorldImagery", group="ESRI Satellite") %>%
		addGraticule(interval=0.5) %>%
  	addRasterImage(projectRaster(habras, crs="+init=epsg:4326", method="ngb"), colors=pal, opacity=habopacity(), group="habitat") %>%
		addPolygons(data=st_transform(bound, "+init=epsg:4326"), weight=2, fill=FALSE) %>%
		addCircleMarkers(data=st_transform(detectors, "+init=epsg:4326"), color="red", radius=1, group="detectors") %>%
		addPolygons(data=st_transform(detector_buff, "+init=epsg:4326"), color="red", weight=1, group="detector buffer") %>%
		addScaleBar("bottomleft", options=scaleBarOptions(imperial=FALSE, maxWidth = 200)) %>%
		addLayersControl(baseGroups=c("OSM (default)", "ESRI Topo", "ESRI Satellite"),
										 overlayGroups = c("detectors", "detector buffer", "habitat"))
})

}

