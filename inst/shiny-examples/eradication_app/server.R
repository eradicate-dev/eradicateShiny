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
#upload rasters of habitat variables.
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

#how many primary sessions?
pperiods<-reactive({
	input$pperiods
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
observe(if(input$Model %in% c("remGRM","remMNS")) {
	updateNumericInput(session, "K", value=5*max(removals()) + 50) #sets default value for K on model select
})

observe(if(input$Model %in% c("remGP","remGPI")) {
	updateNumericInput(session, "K", value=5*max(cedata()$catch) + 50) #sets default value for K on model select
})

observe(if(input$Model %in% c("occMS","remMNS")) {
	if(!is.null(removals()$session))
		updateNumericInput(session, "pperiods", value=unstack.data(removals())$T) #update primary periods
	else
		updateNumericInput(session, "pperiods", value=1)
})

###################################################################################################
#reactive function for habitat value extraction from raster
###################################################################################################
habmean<-reactive({
	rast<- hab_raster()
	buff<- buff()
	traps<- traps()
	traps<- st_as_sf(traps, coords=c(1,2), crs=st_crs(site_bound()))
	if(buff==0){habvals<-terra::extract(rast, vect(traps))} else
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
	modname<- ModToFit()
	modelvars<-input$state_formula
	if(length(modelvars)==0) {form="1"} else
	{form=paste0(modelvars, collapse="+") }
	form<-paste0("~",form)
	if(modname %in% "remMNS")
		form<- paste0(form,"+ .season")
	as.formula(form)
})

DensToPlot<- reactive({
	input$DStype
})

fit_mod<-reactive({
	#fit the selected model to the data.
 cedata <- cedata()
 traps<- traps()
 removals<- removals()
 detections<- detections()
 modname<- ModToFit()
 pperiods<- pperiods()
 K<- K()
       #extract habitat variables only for spatial models, not otherwise
if(modname!="remGP" & modname != "remGPI") {site.data<- habmean()}
if(modname== "remGP") {if("session" %in% colnames(cedata)) {
														emf<- eFrameGP(cedata$catch, cedata$effort, session=cedata$session)
												}
												 else {
												 	 emf<- eFrameGP(cedata$catch, cedata$effort)
												 }
	                       model<- remGP(emf)
	                     } else
if(modname == "remGPI") {
											if("session" %in% colnames(cedata)) {
													emf<- eFrameGP(cedata$catch, cedata$effort, session=cedata$session,
																				index = cedata$index, ieffort =  cedata$ieffort)
											}
											else {
													emf<- eFrameGP(cedata$catch, cedata$effort,
																				 index = cedata$index, ieffort =  cedata$ieffort)
											}
												model<- remGP(emf)
											} else
if(modname== "remMN" )   {emf<-eFrameR(y=removals, siteCovs=site.data, obsCovs=NULL) #specify details
	                       	model<-remMN(lamformula=state_formula(), detformula = ~1, data=emf)
	                       } else
if(modname== "remGRM"  ) {emf<- eFrameGRM(y=removals,     #removal data
																					ym=detections,  #these are secondary monitoring detection data
																					numPrimary=1,
																					siteCovs = site.data)
		                     model<- remGRM(lamformula=state_formula(),
		                     							 phiformula=~1,
		                     							 detformula=~1,
		                     							 mdetformula=~1,
		                     							 data=emf,
		                     							 K=K)} else
if(modname== "remMNS" )  {emf=eFrameMNS(df=removals, siteCovs = site.data)
	                        model=remMNS(lamformula=state_formula(), detformula = ~1, data=emf)} else
if(modname=="occMS")   {emf=eFrameMS(df=removals, siteCovs=site.data)
                         model=occMS(lamformula = state_formula(), gamformula = ~1,
                         						 epsformula= ~1,detformula= ~1, data=emf)}
model
})

#removal plot
removal_plot<-reactive({
	mod_type<- ModToFit()
	if(mod_type == "remGP" | mod_type == "remGPI"){
		plot_data<- cedata()
		if(!("session" %in% colnames(plot_data))) {
			plot_data$session<- 1
			plot_data<- plot_data %>% mutate(cpue = catch/effort, ccatch = cumsum(catch))
		}
		else {
			plot_data<- plot_data %>% group_by(session) %>% mutate(cpue = catch/effort, ccatch = cumsum(catch))
		}
	} else if(mod_type %in% c("remMN","remGRM")) {
		removals<-removals()
		catch<- apply(removals,2,sum,na.rm=TRUE)
		effort<- rep(nrow(removals), length(catch))
		plot_data<- tibble(catch=catch, effort=effort, session=1)
		plot_data<- plot_data %>% mutate(cpue = catch/effort, ccatch = cumsum(catch))
	} else if(mod_type %in% c("remMNS","occMS")){
		removals<- removals()
		tmp<- removals %>% pivot_longer(!session, names_to="period", values_to="catch")
		plot_data<- tmp %>% group_by(session,period) %>% summarise(catch=sum(catch), .groups="keep")
		plot_data<- plot_data %>% group_by(session) %>% mutate(cpue = catch, ccatch=cumsum(catch))
	}
	plot_data %>%
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
	mod_type<- ModToFit()
	if(mod_type == "remGPI") {
		plot_data<- cedata()
		if("index" %in% colnames(plot_data) & "ieffort" %in% colnames(plot_data)) {
		if(!("session" %in% colnames(plot_data))) {
			plot_data$session<- 1
			plot_data<- plot_data %>% mutate(cpue = index/ieffort, ccatch = cumsum(index))
		}
		else {
			plot_data<- plot_data %>% group_by(session) %>% mutate(cpue = index/ieffort, ccatch = cumsum(index))
		}
		}
	}
	else if(mod_type == "remGRM") {
		detections<- detections()
		catch<- apply(detections,2,sum,na.rm=TRUE)
		effort<- rep(nrow(detections), length(catch))
		cpue<- catch/effort
		ccatch<- cumsum(catch)
		plot_data<- tibble(catch=catch, effort=effort, session=1)
		plot_data %>% mutate(cpue = catch/effort, ccatch = cumsum(catch))
	}
		plot_data %>% ggplot(aes(ccatch, cpue)) +
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
	if(mod_type != "remGP" & mod_type != "remGPI"){
	   AIC<-mod$AIC
	   out<-paste0("AIC=",round(AIC, 2))}
	else out<-NULL  #no AIC for remGP
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
	modname<- ModToFit()
	mod<- fit_mod()
	buff<- buff()  #buffer zone radius
	rast<- hab_raster()
	bound<- site_bound()
	DTP<- DensToPlot()
	removals<- removals()
	traps<- traps()
	traps_sf<- st_as_sf(traps, coords=c(1,2), crs=st_crs(bound))
	#get the names of the coefficients that are actually in the model:
	form<- state_formula()
	Irast<- make_dens_surface(rast, mod, modname, form, buff)
	Rrast<- make_resid_dens_surface(Irast, mod, modname, removals, traps_sf, buff)
	if(DTP %in% "IDens") predras<- Irast
	else predras<- Rrast
	predras<- terra::mask(predras, vect(bound))
	predras<- app(predras, calc_min_max)
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
output$download <- downloadHandler(
	filename = function() {
		return('density_raster.tif')
	},
	content = function(file) {
		terra::writeRaster(DensRast(), filename=file, overwrite=TRUE)
	}
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
	if(modname %in% "occMS"){leglab<-"Pr(Occ) for pest"} else {leglab<-"Relative density"}
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

