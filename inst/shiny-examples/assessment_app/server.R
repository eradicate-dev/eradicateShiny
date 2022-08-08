## SERVER eradication monitoring
######################################################

server<-function(input, output, session){
	#splashscreen
	Sys.sleep(1.5) # delay
	waiter_hide() #hide splash screen
#Boundary of study area, in a shapefile
site_bound<-reactive({
	myshape<-input$boundary
	get_zipped_shapefiles(myshape)
	})
#raster of habitat suitability. default is San Nicolas
hab_raster<-reactive({
	if(is.null(input$habitat_rasters))
	{habras<-NULL }
	else
	{ nrast<-nrow(input$habitat_rasters)
	rasts<-list()
	for(i in 1:nrast){rasts[[i]] <- terra::rast(input$habitat_rasters[i, 'datapath'])
	names(rasts[[i]])<-gsub(".tif", "", input$habitat_rasters[i, 'name'])}
	habras<- rast(rasts)
	}
	habras
})


#csv of detectors. default is San Nicolas
detectors<-reactive({
		if(is.null(input$detectors)) {detectors<-NULL} else
			{traps<-read_csv(input$detectors$datapath) }
		traps
	})

#counts
counts<-reactive({
		if(is.null(input$counts)) {counts<-NULL} else
		{counts<-read_csv(input$counts$datapath) }
		counts
	})

#how many primary sessions?
pperiods<-reactive({
	input$pperiods
})

#special reactive inputs for the REST model-----------------------------------------------------------
countREST<-reactive({
	if(is.null(input$countREST)) {countREST<-NULL} else
		{countREST<-read_csv(input$countREST$datapath)}
})

stayREST<-reactive({
	if(is.null(input$stayREST)) {stayREST<-NULL} else
	{stayREST<-data.frame(read_csv(input$stayREST$datapath))}
})

areaREST<-reactive({
  input$areaREST
})

viewshedMultiplier<-reactive({
	input$viewshedMultiplier
})

#habitat radius user input-----------------------------------------------------------------------------
buff<-reactive({
 input$habitat_radius
})

habopacity<-reactive({
	input$Habitat_opacity
})

activeREST<-reactive({
	input$activeREST
})

###################################################################################################
#reactive function for habitat value extraction from raster
###################################################################################################
habmean<-reactive({
	rast<- hab_raster()
	buff<- buff()
	dets<- detectors()
	dets<- st_as_sf(dets, coords=c(1,2), crs=st_crs(site_bound()))
	if(buff==0){
		habvals<-terra::extract(rast, vect(dets))} else
	{
		habvals<-terra::extract(rast, vect(st_buffer(dets, buff)), fun=mean, na.rm=TRUE)
	}
	habvals
})

#reactive to get variables names of habitat rasters
var_names<-reactive({
	rast_names<-names(hab_raster())
	rast_names
})

#observer to update checkboxlist for habitat rasters
observeEvent(var_names(),
						 updateCheckboxGroupInput(session, inputId="state_formula", "habitat covariates",
						 												 choices=var_names()))

observeEvent(var_names(),
						 updateCheckboxGroupInput(session, inputId="state_factors", "habitat factor",
						 												 choices=var_names()))

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
#disable K if using occupancy model
observe(if(input$Model!="Occ" | input$Model != "REST") {
	if(!is.null(counts()))
		updateNumericInput(session, "K", value=5*max(counts()) + 50) #sets default value for K
	else
		updateNumericInput(session, "K", value=50)
})

observe(if(input$Model %in% c("Occ","RN","Nmix")) {
	if("session" %in% colnames(counts()))
		updateNumericInput(session, "pperiods", value=max(counts()$session)) #update primary periods
	else
		updateNumericInput(session, "pperiods", value=1)
})

#############################################################
#  -- Model fitting
#############################################################
ModToFit<-reactive({input$Model})

K<-reactive({input$K})

EstDens<-reactive({input$EstDens})

state_formula<-reactive({
	modname<- ModToFit()
	pperiods=pperiods()
	modelvars<-input$state_formula
	if(length(modelvars)==0) {form="1"} else
	{form=paste0(modelvars, collapse="+") }
	if(pperiods > 1 & !(modname %in% c("REST","Occ")))
		form<- paste0(form,"+ .season")
	form<-paste0("~",form)
	form
	})

#fit the selected model to the data.
fit_mod<-reactive({
	dets<- detectors()
	cnts<-counts()
	habmean<-habmean()
	K<-K()
	pp<- pperiods()
	form<-state_formula()
	modname<-ModToFit()

#fit the appropriate model
if(modname== "Occ" ) {
	if(pp == 1) {
		emf<- eFrame(cnts, siteCovs = habmean)
		model<- occM(form, ~1, data=emf)
	}	else {
		emf<- eFrameMS(cnts, siteCovs = habmean)
		model=occMS(lamformula = form, gamformula = ~1,
								epsformula= ~1,detformula= ~1, data=emf)
	}
} else if(modname== "RN"  ) {
	if(pp == 1) {
		emf<- eFrame(cnts, siteCovs = habmean)
		model<- occRN(form, ~1, K=K, data=emf)
	}	else {
		emf<- eFrameS(cnts, siteCovs = habmean)
		model<- occRNS(form, ~1, K=K, data=emf)
	}
} else if(modname== "Nmix") {
	if(pp == 1) {
		emf<- eFrame(cnts, siteCovs = habmean)
		model<- nmix(form, ~1, K=K, data=emf)
	}	else {
		emf<- eFrameS(cnts, siteCovs = habmean)
		model<- nmixS(form, ~1, K=K, data=emf)
	}
} else if(modname== "REST") {
	Amult<-viewshedMultiplier()
	emf<- eFrameREST(y=countREST(),
									 stay=stayREST()[,1],
									 cens=stayREST()[,2],
									 area=areaREST()/viewshedMultiplier(),
									 active_hours=activeREST(),
									 siteCovs = data.frame(habmean))
	model<-eradicate::REST(state_formula(), data=emf)
}
model
})

#summary table of parameter estimates for selected model
summary_tab<-reactive({
	mod<- fit_mod()
	mod_type<- ModToFit()
	pp<- pperiods()
	out<- make_summary(mod, mod_type, pp)
	return(out)
})


AIC<-reactive({
	mod<-fit_mod()
	AIC<-mod$AIC
	out<-paste0("AIC=",round(AIC, 2))
})

#summary table of abundance estimates conditional on selected model
abund_tab<-reactive({
	modname<-ModToFit()
	mod<-fit_mod()
	buff<-buff()  #buffer zone radius
	pp<- pperiods()
	#If occupancy, no buffer offset applies, else offset estimate with buffer area
	if(modname=="REST"){
  	rast<-hab_raster()
  	newdat<- as.data.frame(rast)
  	offset<- prod(res(rast))/viewshedMultiplier()
  	Nhat<- calcN(mod, newdata=newdat, off.set = offset)
  	out<-Nhat$Nhat
  	} else {
  		out<- make_abund(mod, modname, pp)
  	}
	out
})

abund_plot<- reactive({
	modname<-ModToFit()
	mod<-fit_mod()
	pperiods<- pperiods()
	out<- make_abund(mod, modname, pperiods)
	out<- out %>% mutate(Session=factor(Session))
	out %>% ggplot(aes(Session, N, group=1)) +
		geom_line() +
		geom_linerange(aes(ymin=lcl, ymax=ucl)) +
		geom_point(color="red", size=2.5) +
		labs(x ="Session", y="Abundance (N)") +
		theme_bw() +
		theme(axis.title.x = element_text(face="bold", size=15),
					axis.title.y = element_text(face="bold", size=15),
					axis.text = element_text(size=12))
})

DensRast<-reactive({
	modname<- ModToFit()
	mod<- fit_mod()
	buff<- buff()  #buffer zone radius
	rast<- hab_raster()
	bound<- site_bound()
	pp<- pperiods()
	#get the names of the coefficients that are actually in the model:
	form<- state_formula()
	predras<- make_dens_surface(rast, mod, modname, form, buff)
	predras<- terra::mask(predras, vect(bound))
	predras<- app(predras, calc_min_max)
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
  isolate(summary_tab())
}, row.names=FALSE, width=300, caption="Parameter estimates", caption.placement = "top")

#render table of abundance estimates
output$abundance_table<-renderTable({
	req(input$Run_model)
	isolate(abund_tab())
}, row.names=FALSE, width=300,
   caption="Real estimates",
caption.placement = getOption("xtable.caption.placement", "top"),
caption.width = getOption("xtable.caption.width", NULL))

output$AIC <-renderText({
	req(input$Run_model)
	isolate(AIC())
})

output$abund_plot<- renderPlot({
	req(input$Run_model)
	isolate(abund_plot())
})

#download the density raster
output$downloadraster <- downloadHandler(
	filename = "density_raster.tif",
	content = function(file) {
		writeRaster(DensRast(), filename=filename, format="GTiff", overwrite=TRUE)
	}, contentType = "image/tif"
)

output$map<-renderLeaflet({
	req(input$boundary)
	req(input$Plot_design)
	req(input$Habitat_opacity)
	req(input$habitat_radius)
	#set up and tranform site boundary
	bound<-site_bound()
	opacity<-habopacity()
	transparency<-1-opacity
	detectors<- detectors()
	buffer<- buff()
	habras<-hab_raster()
	m<- make_leaflet_map(bound, habras, detectors, buffer, transparency)
})

#code to update map with raster predictions
observeEvent(input$EstDens, {
	req(input$EstDens)
	req(input$boundary)
	req(input$Habitat_opacity)
	req(input$Run_model)
	bound<-site_bound()
	modname<-ModToFit()
	if(modname %in% "Occ"){leglab<-"Pr(Occ) for pest"} else {leglab<-"Relative density"}
	DensRast <- DensRast()
	opacity<-habopacity()
	transparency<-1-opacity
	crs(DensRast)<- st_crs(bound)$wkt
	DensRastProj<- terra::project(DensRast, "epsg:4326", method="bilinear")
	#need to reimport habitat raster
	habras<-hab_raster()
	crs(habras)<- st_crs(bound)$wkt #assume same crs as region boundary
	habrasproj<- terra::project(habras, "epsg:4326", method="bilinear")
	DensRastProj<- raster::raster(DensRastProj)
	habrasproj<- raster::raster(habrasproj)
	vrange<- range(values(DensRastProj),na.rm=TRUE)
	fuzz<- diff(vrange) * 0.01  # add small value so that min and max values appear in plot
	vrange[1]<- vrange[1] - fuzz
	vrange[2]<- vrange[2] + fuzz
	pal <- colorNumeric("Reds", vrange, na.color = "transparent")
	leafletProxy("map") %>%
		clearGroup("density raster") %>%
		removeControl("denslegend") %>%
		addRasterImage(DensRastProj, colors=pal, layerId = "PestDensity",
									 opacity=transparency, group="density raster") %>%
		addLegend(position="bottomright", layerId = "denslegend", pal=pal, bins=6, title=leglab, values=values(DensRastProj)) %>%
		addLayersControl(baseGroups=c("OSM (default)", "ESRI Topo", "ESRI Satellite"),
										 overlayGroups = c("traps", "traps buffer", names(habrasproj), "density raster"),
										 options=layersControlOptions(collapsed=FALSE)) %>%
		hideGroup(c("traps buffer", "traps", names(habrasproj)))
})




} #end server

