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
#upload rasters of habitat variables. default is San Nicolas
	hab_raster<-reactive({
		if(is.null(input$habitat_rasters))
		{habras<-stack(system.file("extdata", "san_nic_habitat.tif", package="eradicate"))
		names(habras)<-"san_nic_habitat" } else
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
###################################################################################################
#reactive function for habitat value extraction from raster
###################################################################################################
habmean<-reactive({
	rast<-hab_raster()
	buff<-buff()
	traps<-traps()
	habvals<-raster::extract(rast, traps, buffer=buff, fun=mean)
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


state_formula<-reactive({
	modelvars<-input$state_formula
	if(length(modelvars)==0) {form="1"} else
	{form=paste0(modelvars, collapse="+") }
	form<-paste0("~",form)
	form
})

#fit the selected model to the data.
fit_mod<-reactive({
       traps<- traps()
       removals<-removals()
       detections<-detections()
       nights<-nights()
       modname<-ModToFit()
       K<-K()
       #extract habitat variables only for spatial models, not otherwise
       if(modname!="remCE" & modname!="remGP") {site.data<-habmean()}
if(modname== "remCE")    {catch<- apply(removals,2,sum)
                          effort<- rep(nrow(removals), length(catch))
	                        model<-remCE(catch, effort)}  else
if(modname== "remGP")    {catch<- apply(removals,2,sum)
	                        effort<- rep(nrow(removals), length(catch))
	                       # extra monitoring/effort data
	                       index<- apply(detections,2,sum)
	                       ieffort<- rep(nrow(detections), length(index))
	                       emf<- eFrameGP(catch, effort, index, ieffort)
	                       model<- remGP(emf) } else
if(modname== "remGR")    {emf<- eFrameGR(removals,  numPrimary=1, type="removal", siteCovs = site.data)
		                     model<- eradicate::remGR(state_formula(), ~1, ~1, data=emf, K=K)} else
if(modname== "remGRM"  ) {emf<- eFrameGRM(removals, detections, numPrimary=1, type="removal", siteCovs = site.data)
		                     model<- eradicate::remGRM(state_formula(), ~1, ~1, ~1, data=emf, K=K)} else
if(modname== "remMN" )   {emf<-eFrameR(y=removals, siteCovs=site.data, obsCovs=NULL) #specify details
	                        model<-remMN(lamformula=state_formula(), detformula = ~1, data=emf)} else
if(modname== "remMNO" )  {emf=eFrameMNO()#specify details
	                        model=remMN(lamformula=state_formula(), gamformula=~1, data=emf,
	                        						 omformula=~1, detformula = ~1, data=emf, K=K)}
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
	opacity<-habopacity()
	transparency<-1-opacity

	traps<-st_as_sf(traps(), coords = c("x", "y"), crs=st_crs(bound))
	traps_buff<-st_buffer(traps, dist=buff())
	habras<-hab_raster()
	crs(habras)<-crs(bound) #assume same crs as region boundary
	habrasproj<-projectRaster(habras, crs="+init=epsg:4326", method="bilinear")
	nrast<-nlayers(habrasproj) #how many habitat rasters in the stack?
	palvec<-c("viridis", "magma", "plasma", "inferno")
	m<-leaflet() %>%
		addTiles(group="OSM") %>%
		addProviderTiles("Esri.WorldTopoMap", group="ESRI Topo") %>%
		addProviderTiles("Esri.WorldImagery", group="ESRI Satellite")
	count =1
	while(count<=nrast) {  #adding habitat rasters one at a time
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
	m <- m %>%	addPolygons(data=st_transform(bound, "+init=epsg:4326"), weight=2, fill=FALSE) %>%
		addCircleMarkers(data=st_transform(traps, "+init=epsg:4326"), color="red", radius=1, group="traps") %>%
		addPolygons(data=st_transform(traps_buff, "+init=epsg:4326"), color="red", weight=1, group="traps buffer") %>%
		addScaleBar("bottomright", options=scaleBarOptions(imperial=FALSE, maxWidth = 200)) %>%

		addLayersControl(baseGroups=c("OSM (default)", "ESRI Topo", "ESRI Satellite"),
										 overlayGroups = c("traps", "traps buffer", names(habrasproj)),
										 options=layersControlOptions(collapsed=FALSE)) %>%
		hideGroup("traps buffer") %>%
		hideGroup(names(habrasproj)[-1]) #keep the first raster displayed
})

}

