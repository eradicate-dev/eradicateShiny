##DEFINE THE SERVER#####################################################################################################
server<-function(input, output, session){
	#splashscreen
	Sys.sleep(4.5) # delay
	waiter_hide() #hide splash screen
#Boundary of study area, in a shapefile
	site_bound<-reactive({
		myshape<-input$boundary
		get_zipped_shapefiles(myshape)
	})
#upload rasters of habitat variables. default is San Nicolas
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
#csv of non-spatial catch effort data
cedata<- reactive({
	load_file(input$cedata$name, input$cedata$datapath)
	})

#csv of trap locations.
traps<-reactive({
	load_file(input$traps$name, input$traps$datapath)
	})

#removals
removals<-reactive({
	load_file(input$removals$name, input$removals$datapath)
	})

#passive detections
detections<-reactive({
	load_file(input$detections$name, input$detections$datapath)
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
observeEvent(input$EstDens, {
	updateTabsetPanel(session, "maintabs",
										selected = "panel1")
})
observe(if(input$Model %in% c("remGRM","remMNO")) {
	updateNumericInput(session, "K", value=5*max(removals()) + 50) #sets default value for K on model select
})

observe(if(input$Model %in% c("remGP")) {
	updateNumericInput(session, "K", value=5*max(cedata()$catch) + 50) #sets default value for K on model select
})

###################################################################################################
#reactive function for habitat value extraction from raster
###################################################################################################
habmean<-reactive({
	rast<-hab_raster()
	buff<-buff()
	traps<-traps()
	traps<- st_as_sf(traps, coords=c(1,2), crs=st_crs(site_bound()))
	if(buff==0){habvals<-terra::extract(rast, traps)} else
	{ traps<- st_buffer(traps, buff)
		habvals<-terra::extract(rast, vect(traps), fun=mean, na.rm=TRUE)
		}
	habvals
})

#reactive to get the variables names of the habitat rasters
var_names<-reactive({
	rast_names<-names(hab_raster())
	rast_names
})

#observer to update checkboxlist for the current habitat rasters
observeEvent(var_names(),
						 updateCheckboxGroupInput(session, inputId="state_formula", "habitat covariates",
						 												 choices=var_names()))

#############################################################
#  -- Model fitting options
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

state_formula<-reactive({
	modelvars<-input$state_formula
	if(length(modelvars)==0) {form="1"} else
	{form=paste0(modelvars, collapse="+") }
	form<-paste0("~",form)
	form
})


fit_mod<-reactive({
	#fit the selected model to the data.
 cedata <- cedata()
 traps<- traps()
 removals<- removals()
 detections<- detections()
 modname<- ModToFit()
 nights<- nights()
 K<- K()

       #extract habitat variables only for spatial models, not otherwise
if(modname!="remGP") {site.data<-habmean()}
if(modname== "remGP") {if("session" %in% colnames(cedata))
														emf<- eFrameGP(cedata$catch, cedata$effort, session=cedata$session)
												 else
												 		emf<- eFrameGP(cedata$catch, cedata$effort)
	                       model<- remGP(emf)
	                       } else
if(modname== "remMN" )   {emf<-eFrameR(y=removals, siteCovs=site.data, obsCovs=NULL) #specify details
	                       	model<-remMN(lamformula=state_formula(), detformula = ~1, data=emf)
	                       } else
if(modname== "remGRM"  ) {emf<- eFrameGRM(y=removals,     #removal data
																					ym=detections,  #these are secondary monitoring detection data
																					numPrimary=1,
																					siteCovs = site.data)
		                     model<- remGRM(lamformula=as.formula(state_formula()),
		                     							 phiformula=~1,
		                     							 detformula=~1,
		                     							 mdetformula=~1,
		                     							 data=emf,
		                     							 K=K)} else
if(modname== "remMNO" )  {emf=eFrameMNO()#specify details of data structure TBD
	                        model=remMNO(lamformula=state_formula(), gamformula=~1,
	                        						 omformula=~1, detformula = ~1, data=emf, K=K)} else
if(modname=="occuMS")   {emf=eframeMS() #specify details of data structure TBD
                         model=occuMS(psiformula = state_formula, gamformula = ~1,
                         						 epsformulat=~1,detformula=~1, data=emf)}
model
})

#removal plot
removal_plot<-reactive({
	mod_type<- ModToFit()
	if(mod_type == "remGP"){
		plot_data<- cedata()
		if(!("session" %in% colnames(plot_data)))
			plot_data$session<- 1
	} else if(mod_type %in% c("remMN","remGRM")) {
		removals<-removals()
		catch<- apply(removals,2,sum,na.rm=TRUE)
		effort<- rep(nrow(removals), length(catch))
		plot_data<- tibble(catch=catch, effort=effort, session=1)
	}
	plot_data %>% mutate(cpue = catch/effort, ccatch = cumsum(catch)) %>%
		ggplot(aes(ccatch, cpue)) +
		geom_point() +
		geom_smooth(method="lm") +
		facet_wrap(~session, scales="free") +
		labs(x ="Cumulative removals", y="CPUE") +
		theme_bw() +
		theme(axis.title.x = element_text(face="bold", size=15),
					axis.title.y = element_text(face="bold", size=15),
					axis.text = element_text(size=12),
					legend.position = "bottom")
})

detection_plot<-reactive({
	detections<- detections()
	mod_type<- ModToFit()
	catch<- apply(detections,2,sum,na.rm=TRUE)
	effort<- rep(nrow(detections), length(catch))
	cpue<- catch/effort
	ccatch<- cumsum(catch)
	plot_data<- tibble(catch=catch, effort=effort, session=1)
	plot_data %>% mutate(cpue = catch/effort, ccatch = cumsum(catch)) %>%
		ggplot(aes(ccatch, cpue)) +
		geom_point() +
		geom_smooth(method="lm") +
		facet_wrap(~session, scales="free") +
		labs(x ="Cumulative detections", y="DPUE") +
		theme_bw() +
		theme(axis.title.x = element_text(face="bold", size=15),
					axis.title.y = element_text(face="bold", size=15),
					axis.text = element_text(size=12),
					legend.position = "bottom")
})

abund_plot<- reactive({
	modname<-ModToFit()
	mod<-fit_mod()
	out<- make_abund(mod, modname)
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
#summary table of parameter estimates for selected model
summary_tab<-reactive({
	mod<- fit_mod()
	mod_type<- ModToFit()
	out<- make_summary(mod, mod_type)
	return(out)
})

AIC<-reactive({
	mod<-fit_mod()
	mod_type<-ModToFit()
	if(mod_type!="remGP"){
	   AIC<-mod$AIC
	   out<-paste0("AIC=",round(AIC, 2))} else
	if(mod_type=="remGP"){out<-NULL}  #no AIC for remGP
	  return(out)
})

#summary table of abundance estimates conditional on selected model
abund_tab<-reactive({
	modname<-ModToFit()
	mod<-fit_mod()
	buff<-buff()  #buffer zone radius (for later implementation of extrapolation calculation)
	out<- make_abund(mod, modname)
	return(out)
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
	print("estimating density surface")
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
# Event reactives so only react on button press
summary_tab_react<- eventReactive(input$Run_model, {summary_tab()})

removal_plot_react<- eventReactive(input$Plot_removal,{removal_plot()})

#render a table of parameter estimates
output$parameter_table<-renderTable({
	summary_tab_react()
}, row.names=FALSE, width=300, caption="Parameter estimates", caption.placement = "top")


output$removal_plot<-renderPlot({
	#isolate(removal_plot())
	removal_plot_react()
})

output$detection_plot<-renderPlot({
	isolate(detection_plot())
})

#render table of abundance estimates
output$abundance_table<-renderTable({
	req(input$Run_model)
	isolate(abund_tab())
}, row.names=FALSE, width=300, caption="Abundances", caption.placement = "top")

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
  traps<-traps()
  buffer<- buff()
  habras<-hab_raster()
  m<- make_leaflet_map(bound, habras, traps, buffer, transparency)
})

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
										 overlayGroups = c("traps", "traps buffer", names(habrasproj), "density raster"),
										 options=layersControlOptions(collapsed=FALSE)) %>%
		hideGroup(c("traps buffer", "traps", names(habrasproj)))
})

} #end server

