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
		{habras<-NULL } else
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
			{traps<-NULL} else
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
observeEvent(input$EstDens, {
	updateTabsetPanel(session, "maintabs",
										selected = "panel1")
})
observe(if(input$Model!="remMN" & input$Model!="occuMS" ) {
	updateNumericInput(session, "K", value=5*max(detections()) + 50) #sets default value for K on model select
})
###################################################################################################
#reactive function for habitat value extraction from raster
###################################################################################################
habmean<-reactive({
	rast<-hab_raster()
	buff<-buff()
	traps<-traps()
	if(buff==0){habvals<-raster::extract(rast, traps, df=TRUE)} else
	{habvals<-raster::extract(rast, traps, buffer=buff, fun=mean, df=TRUE)}
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
       traps<- traps()
       removals<-removals()
       detections<-detections()
       nights<-nights()
       modname<-ModToFit()
       K<-K()
       #extract habitat variables only for spatial models, not otherwise
       if(modname!="remCE" & modname!="remGP") {site.data<-habmean()}
if(modname== "remCE")    {catch<- apply(removals,2,sum)
                          effort<- colSums(!is.na(removals)) #number of non NA removal results at each time
	                        model<-remCE(catch, effort, nboot=50)}  else
if(modname== "remGP")    {catch<- apply(removals,2,sum)
	                        effort<- rep(nrow(removals), length(catch))
	                       # extra monitoring/effort data
	                       index<- apply(detections,2,sum)
	                       ieffort<- rep(nrow(detections), length(index))
	                       emf<- eFrameGP(catch, effort, index, ieffort)
	                       model<- remGP(emf) } else
if(modname== "remGR")    {emf<- eFrameGR(y=removals,
																				 numPrimary=1,
																				 siteCovs = site.data)
		                     model<- remGR(lamformula=as.formula(state_formula()), #weird requirement to make the formula string a formula!
		                     							phiformula = ~1,
		                     							detformula = ~1,
		                     							data=emf,
		                     							K=K)
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
if(modname== "remMN" )   {emf<-eFrameR(y=removals, siteCovs=site.data, obsCovs=NULL) #specify details
	                        model<-remMN(lamformula=state_formula(), detformula = ~1, data=emf)} else
if(modname== "remMNO" )  {emf=eFrameMNO()#specify details of data structure TBD
	                        model=remMN(lamformula=state_formula(), gamformula=~1,
	                        						 omformula=~1, detformula = ~1, data=emf, K=K)} else
if(modname=="occuMS")   {emf=eframeMS() #specify details of data structure TBD
                         model=occuMS(psiformula = state_formula, gamformula = ~1,
                         						 epsformulat=~1,detformula=~1, data=emf)}
model
})

#removal plot
removal_plot<-reactive({
	removals<-removals()
	catch<- apply(removals,2,sum,na.rm=TRUE)
	effort<- rep(nrow(removals), length(catch))
	cpue<- catch/effort
	ccatch<- cumsum(catch)
	m<- coef(lm(cpue ~ ccatch))
	plot(x=ccatch, y=cpue, type="p", col="red", las=1, pch=16,
			 xlab="CPUE", ylab="Cumulative removals", main="Cumulative catch vs CPUE")
	abline(a=m[1],b=m[2])
})

detection_plot<-reactive({
	detections<-detections()
	catch<- apply(detections,2,sum,na.rm=TRUE)
	effort<- rep(nrow(detections), length(catch))
	cpue<- catch/effort
	ccatch<- cumsum(catch)
	m<- coef(lm(cpue ~ ccatch))
	plot(x=ccatch, y=cpue, type="p", col="red", las=1, pch=16,
			 xlab="CPUE", ylab="Cumulative detections", main="Cumulative detections")
	abline(a=m[1],b=m[2])
})



#summary table of parameter estimates
#summary table of parameter estimates for selected model
summary_tab<-reactive({
	mod<-fit_mod()
	mod_type<-ModToFit()
	if(mod_type!="remCE"){
	out<-summary(mod)
	state_tab<-out[[1]]
	state_tab<-data.frame("Type"="State","Covariate"=row.names(state_tab), state_tab)
	detect_tab<-out[[2]]
	detect_tab<-data.frame("Type"="Detect","Covariate"=row.names(detect_tab), detect_tab)
	out<-rbind(state_tab, detect_tab)
	data.frame(out)
	names(out)[6]<-"p"} else
   if(mod_type=="remCE"){  #CE model doesn't have a summary method
   	out<-data.frame(mod$results)[2,]
   	out<-data.frame("Parameter"="lambda", out)
   	names(out)<-c("Parameter", "Estimate", "SE", "Lwr95", "Upp95", "CV")
   	out<-out[,1:3]  #hide unwanted columns for now
   }
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
	if(modname=="remCE") {out<-data.frame(mod$results)[1,]
	                      out<-data.frame("Parameter"="N", out) } else
          {out<-calcN(mod)
		       tmp<-rbind(out$Nhat, out$Nresid)
           out<-data.frame("Parameter"=c("Nhat", "Nresid"), tmp, "CV"=NA)
          }
	names(out)<-c("Parameter", "Estimate", "SE", "Lwr95", "Upp95", "CV")
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

#render a table of parameter estimates
output$parameter_table<-renderTable({
req(input$Run_model)
  isolate(summary_tab())
}, row.names=FALSE, width=300, caption="Parameter estimates", caption.placement = "top")

output$removal_plot<-renderPlot({
	isolate(removal_plot())
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
  if(!is.null(traps)){
	traps<-st_as_sf(traps, coords = c(1, 2), crs=st_crs(bound))
	traps_buff<-st_buffer(traps, dist=buff())
  } else {traps<-NULL; traps_buff<-NULL}
	habras<-hab_raster()
	if(!is.null(habras)){
	crs(habras)<-crs(bound) #assume same crs as region boundary
	habrasproj<-projectRaster(habras, crs="+init=epsg:4326", method="bilinear")
	nrast<-nlayers(habrasproj)} else {nrast<-0; habrasproj<-NULL} #how many habitat rasters in the stack?
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

