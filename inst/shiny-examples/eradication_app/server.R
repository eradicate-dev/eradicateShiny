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
#csv of trap locations.
traps<-reactive({
		if(is.null(input$traps))
			{traps<-eradicate::san_nic_rem$traps} else
			{traps<-read_csv(input$traps$datapath) }
		traps
	})

#removals
removals<-reactive({
		if(is.null(input$removals))
		{removals<-eradicate::san_nic_rem$rem} else
		{removals<-read_csv(input$removals$datapath) }
		removals
	})

#passive detections
detections<-reactive({
	if(is.null(input$detections))
	{detections<-eradicate::san_nic_rem$ym} else
	{detections<-read_csv(input$detections$datapath) }
	detections
})

#how many nights per primary session?
nights<-reactive({
	input$nights
})
#habitat radius user input
buff<-reactive({
 input$habitat_radius
})

habopacity<-reactive({
	input$Habitat_opacity
})

K<-reactive({
	input$K
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
observeEvent(input$Plot_removal, {
	updateTabsetPanel(session, "maintabs",
										selected = "panel3")
})
observe(if(input$Model!="RemPois" & input$Model!="RemGP") {
	updateNumericInput(session, "K", value=5*max(detections()) + 50) #sets default value for K on model select
})

#############################################################
#  -- Model fitting options
#############################################################
ModToFit<-reactive({
	input$Model
})

#fit the selected model to the data.
fit_mod<-reactive({
traps<- traps()
rast<-hab_raster()
removals<-removals()
detections<-detections()
nights<-nights()
buff<-buff()
modname<-ModToFit()
K<-K()
#prep the data
habvals<-raster::extract(rast, traps, buffer=buff)
habmean<- sapply(habvals, function(x) mean(x, na.rm=T))
site.data<- cbind(traps, habmean)
if(modname== "remPois" ) {emf<-eFrameR(removals,type="removal", siteCovs = site.data)
	                         model<-eradicate::remPois(~habmean, ~1, data=emf)} else
if(modname== "remGR"  ) {emf<- eFrameGR(removals,  numPrimary=1, type="removal", siteCovs = site.data)
                           model<- eradicate::remGR(~habmean, ~1, ~1, data=emf, K=K)} else
if(modname== "remGRM"  ) {emf<- eFrameGRM(removals, detections, numPrimary=1, type="removal", siteCovs = site.data)
                           	model<- eradicate::remGRM(~habmean, ~1, ~1, ~1, data=emf, K=K)} else
if(modname== "remGP"  ) {catch<- apply(removals,2,sum)
                          effort<- rep(nrow(removals), length(catch))
                         # extra monitoring/effort data
                          index<- apply(detections,2,sum)
                          ieffort<- rep(nrow(detections), length(index))
                          emf<- eFrameGP(catch, effort, index, ieffort)
                          model<- remGP(emf)
}
model
})

#removal plot
removal_plot<-reactive({
	removals<-removals()
	detections<-detections()
	y=c(0, cumsum(colSums(removals)))
	x=0:ncol(removals)
	plot(y=y, x=x, type="o", col="red", las=1, pch=16, ylim=c(0, max(y)),
			 xlab="Primary Sessions", ylab="Cumulative removals", main="Cumulative removals")
})

detection_plot<-reactive({
	removals<-removals()
	detections<-detections()
	y=c(0, cumsum(colSums(detections)))
	x=0:ncol(detections)
	plot(y=y, x=x, type="o", col="blue", las=1, pch=16, ylim=c(0, max(y)),
			 xlab="Primary Sessions", ylab="Cumulative detections", main="Cumulative detections")
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
	ests<-calcN(mod)
	Nhat<-ests$Nhat
	Nresid<-ests$Nresid
	out<-bind_rows(Nhat, Nresid)
	out<-data.frame(out)#data.frame("Parameter"=c("Nhat", "Nresid"), out)
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

output$removal_plot<-renderPlot({
	removal_plot()
})

output$detection_plot<-renderPlot({
	detection_plot()
})

#render table of abundance estimates
output$abundance_table<-renderTable({
	req(input$Run_model)
	abund_tab()
}, row.names=FALSE, width=300, caption="Abundances", caption.placement = "top")

#Render a map of the input data
output$map<-renderLeaflet({
	req(input$boundary)
	req(input$Plot_design)
	req(input$Habitat_opacity)
	req(input$habitat_radius)
	#set up and tranform site boundary
	bound<-site_bound()
	traps<-st_as_sf(traps(), coords = c("x", "y"), crs=st_crs(bound))
	traps_buff<-st_buffer(traps, dist=buff())
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
		addCircleMarkers(data=st_transform(traps, "+init=epsg:4326"), color="red", radius=1, group="traps") %>%
		addPolygons(data=st_transform(traps_buff, "+init=epsg:4326"), color="red", weight=1, group="traps buffer") %>%
		addScaleBar("bottomleft", options=scaleBarOptions(imperial=FALSE, maxWidth = 200)) %>%
		addLayersControl(baseGroups=c("OSM (default)", "ESRI Topo", "ESRI Satellite"),
										 overlayGroups = c("traps", "traps buffer", "habitat raster"),
										 options=layersControlOptions(collapsed=FALSE))
})

}

