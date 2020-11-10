##DEFINE THE SERVER#####################################################################################################
server<-function(input, output, session){
	#splashscreen
	Sys.sleep(4.5) # delay
	waiter_hide() #hide splash screen
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
	if(is.null(input$habitat_rasters)) {habras<-NULL } else
	{
		#habras<-stack(input$habitat_raster$datapath)
		nrast<-nrow(input$habitat_rasters)
		rasts<-list()
		for(i in 1:nrast){rasts[[i]] <- raster(input$habitat_rasters[i, 'datapath'])
		names(rasts[[i]])<-gsub(".tif", "", input$habitat_rasters[i, 'name'])}
		habras<-stack(rasts)
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

#special reactive inputs for the REST model-----------------------------------------------------------
countREST<-reactive({
	if(is.null(input$countREST)) {countREST<-NULL} else
		{countREST<-read_csv(input$countREST$datapath)}
})

stayREST<-reactive({
	if(is.null(input$stayREST)) {stayREST<-NULL} else
	{stayREST<-read_csv(input$stayREST$datapath)}
})

censREST<-reactive({
	if(is.null(input$censREST)) {censREST<-NULL} else
	{censREST<-read_csv(input$censREST$datapath)}
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
	rast<-hab_raster()
	buff<-buff()
	dets<-detectors()
	habvals<-raster::extract(rast, dets, buffer=buff, fun=mean, df=TRUE)
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
observe(if(input$Model!="Occ") {
	updateNumericInput(session, "K", value=5*max(counts()) + 50) #sets default value for K
})

#############################################################
#  -- Model fitting
#############################################################
ModToFit<-reactive({input$Model})

K<-reactive({input$K})

EstDens<-reactive({input$EstDens})

state_formula<-reactive({
	modelvars<-input$state_formula
	if(length(modelvars)==0) {form="1"} else
	                         {form=paste0(modelvars, collapse="+") }
	form<-paste0("~",form)
	form
	})

#fit the selected model to the data.
fit_mod<-reactive({
	dets<- detectors()
	cnts<-counts()
	habmean<-habmean()
	print(habmean)
	K<-K()
	form<-state_formula()
modname<-ModToFit()
if(modname!= "REST"){emf<- eradicate::eFrame(cnts, siteCovs = data.frame(habmean))} else
                    {
                      Amult<-viewshedMultiplier()
                    	emf<- eFrameREST(y=countREST(),
                    									stay=stayREST(),
                    									cens=censREST(),
                    									area=areaREST()/viewshedMultiplier(),
                    									active_hours=activeREST(),
                    									siteCovs = data.frame(habmean))}
#fit the appropriate model
if(modname== "Occ" ) {model<-eradicate::occuM(state_formula(), ~1, data=emf)}       else
if(modname== "RN"  ) {model<-eradicate::occuRN(state_formula(), ~1, K=K, data=emf)} else
if(modname== "Nmix") {model<-eradicate::nmix(state_formula(), ~1, K=K, data=emf)}   else
if(modname== "REST") {model<-eradicate::REST(state_formula(), data=emf)}
model
})

#summary table of parameter estimates for selected model
summary_tab<-reactive({
mod<-fit_mod()
out<-summary(mod)
state_tab<-out[[1]]
state_tab<-data.frame("Type"="State","Covariate"=row.names(state_tab), state_tab)
detect_tab<-out[[2]]
detect_tab<-data.frame("Type"="Detect","Covariate"=row.names(detect_tab), detect_tab)
out<-rbind(state_tab, detect_tab)
data.frame(out)
names(out)[6]<-"p"
out
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
	#If occupancy, no buffer offset applies, else offset estimate with buffer area
	if(modname=="Occ") {Nhat<-calcN(mod)} else {Nhat<-calcN(mod)}
  if(modname=="Occ"){out<-Nhat$Occ} else
  if(modname=="RN"){out<-Nhat$Nhat} else
  if(modname=="Nmix"){out<-Nhat$Nhat} else
  if(modname=="REST"){out<-Nhat$Nhat}
	out
})

DensRast<-reactive({
	modname<-ModToFit()
	mod<-fit_mod()
	buff<-buff()  #buffer zone radius
	rast<-hab_raster()
	bound<-site_bound()
	#get the names of the coefficients that are actually in the model:
	form<-state_formula()
	form_names<-gsub("~", "", form)
	form_names<-strsplit(form_names, "\\+")
	#
	coeffs<-mod$estimates$state$estimates
	varnames<-names(rast)

	#if intercept only, don't bother with focal raster calculations
	if(form== "~1"){preds.lin<-coeffs[1]} else {
      #calculate focal rasters, but only for required coefficients
		  rast_incl<-which(varnames %in% names(coeffs))
		  rast_use<-rast[[rast_incl]]
	    filt<-focalWeight(x=rast_use, d=buff, type='circle')
	    rastfocal<-list()
	      for(i in seq_along(names(rast_use))){
	  	  #make a focal layer for each raster in the stack
	      rastfocal[[i]]<-focal(rast_use[[i]], w=filt,fun=mean, na.rm=TRUE, pad=TRUE)
	      } #end focal loop
	  rastfocal<-stack(rastfocal)
	vals<-getValues(rastfocal)
	vals<-cbind(1, vals) #add an intercept
	preds.lin<-vals %*% coeffs
	}
	#back transform from link scale.
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

output$AIC <-renderText({
	req(input$Run_model)
	AIC()
})

#download the density raster
output$downloadraster <- downloadHandler(
	filename = "density_raster.tif",
	content = function(file) {
		writeRaster(DensRast(), filename=filename, format="GTiff", overwrite=TRUE)
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
	opacity<-habopacity()
	transparency<-1-opacity
	detectors<-st_as_sf(detectors(), coords = c("x", "y"), crs=st_crs(bound))
	detector_buff<-st_buffer(detectors, dist=buff())
	habras<-hab_raster()
  crs(habras)<-crs(bound) #assume same crs as region boundary
  habrasproj<-projectRaster(habras, crs="+init=epsg:4326", method="bilinear")
  nrast<-nlayers(habrasproj) #how many habitat rasters in the stack?
	palvec<-c("viridis", "magma", "plasma", "inferno")
  m<-leaflet() %>%
		addTiles(group="OSM") %>%
		addProviderTiles("Esri.WorldTopoMap", group="ESRI Topo") %>%
		addProviderTiles("Esri.WorldImagery", group="ESRI Satellite")
  # conditional addition of extra raster layers to the plot
  count=1
  while(count<=nrast) {
  	m <- m %>% addRasterImage(habrasproj[[count]],
  														colors=colorNumeric(palvec[count], values(habrasproj[[count]]), na.color = "#00000000"),
  														opacity=transparency,
  														layerId = names(habrasproj)[count],
  														group=names(habrasproj)[count]) %>%
  		         addLegend(values=values(habrasproj[[count]]) ,
  		         					pal = colorNumeric(palvec[count], values(habrasproj[[count]]), na.color = "#00000000"),
  		         					title=names(habrasproj)[count], bins=5,
  							        group = names(habrasproj)[count], position="bottomleft")

  	count<-count+1
  }
  #adding additional decorations to the plot
	m<-m %>%
		addPolygons(data=st_transform(bound, "+init=epsg:4326"), weight=2, fill=FALSE) %>%
		addCircleMarkers(data=st_transform(detectors, "+init=epsg:4326"), color="red", radius=1, group="detectors") %>%
		addPolygons(data=st_transform(detector_buff, "+init=epsg:4326"), color="red", weight=1, group="detector buffer") %>%
		addScaleBar("bottomright", options=scaleBarOptions(imperial=FALSE, maxWidth = 200)) %>%
		addLayersControl(baseGroups=c("OSM (default)", "ESRI Topo", "ESRI Satellite"),
										 overlayGroups = c("detectors", "detector buffer", names(habrasproj)),
										 options=layersControlOptions(collapsed=FALSE)) %>%
		hideGroup("detector buffer") %>%
		hideGroup(names(habrasproj)[-1]) #keep the first raster displayed
	m
}
)

#code to update map with raster predictions
observeEvent(input$EstDens, {
	req(input$EstDens)
	req(input$boundary)
	req(input$Habitat_opacity)
	req(input$Run_model)
	bound<-site_bound()
	modname<-ModToFit()
	if(modname=="Occ"){leglab<-"Pr(Occ) for pest"} else {leglab<-"Pest density"}
	DensRast <- DensRast()
	opacity<-habopacity()
	transparency<-1-opacity
	crs(DensRast)<-crs(bound)
	DensRastProj<-projectRaster(DensRast, crs="+init=epsg:4326", method="bilinear")
	#need to reimport habitat raster
	habras<-hab_raster()
	crs(habras)<-crs(bound) #assume same crs as region boundary
	habrasproj<-projectRaster(habras, crs="+init=epsg:4326", method="bilinear")
	pal <- colorNumeric("Reds", values(DensRastProj), na.color = "transparent")
	leafletProxy("map") %>%
		clearGroup("density raster") %>%
		removeControl("denslegend") %>%
		addRasterImage(DensRastProj, colors=pal, layerId = "PestDensity",
							opacity=transparency, group="density raster") %>%
		addLegend(position="bottomright", layerId = "denslegend", pal=pal, bins=6, title=leglab, values=values(DensRastProj)) %>%
		addLayersControl(baseGroups=c("OSM (default)", "ESRI Topo", "ESRI Satellite"),
										 overlayGroups = c("detectors", "detector buffer", names(habrasproj), "density raster"),
										 options=layersControlOptions(collapsed=FALSE)) %>%
		hideGroup(c("detector buffer", "detectors"))
})



} #end server

