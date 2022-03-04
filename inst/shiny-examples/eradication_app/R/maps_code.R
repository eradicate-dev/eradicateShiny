
# get_shapefiles<- function(myshape) {
# 	dir<-dirname(myshape$datapath[1])
# 	for (i in 1:nrow(myshape)) {
# 		file.rename(myshape$datapath[i], paste0(dir,"/",myshape$name[i]))
# 	}
# 	getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
# 	shp<- st_read(getshp, quiet=TRUE)
# 	return(shp)
# }

 get_shapefiles<- function(myshape) {
 	dir<-dirname(myshape$datapath[1])
 	unzip(myshape$datapath, exdir = dir)
 	getshp <- list.files(dir, pattern="*.shp", full.names=TRUE)
 	shp<- st_read(getshp, quiet=TRUE)
 	return(shp)
 }

# #create a couple temp files
# temp <- tempfile()
# temp2 <- tempfile()
# #download the zip folder from the internet save to 'temp'
# download.file("https://www2.census.gov/geo/tiger/TIGER2017/COUNTY/tl_2017_us_county.zip",temp)
# #unzip the contents in 'temp' and save unzipped content in 'temp2'
# unzip(zipfile = temp, exdir = temp2)
# #finds the filepath of the shapefile (.shp) file in the temp2 unzip folder
# #the $ at the end of ".shp$" ensures you are not also finding files such as .shp.xml
# your_SHP_file<-list.files(temp2, pattern = ".shp$",full.names=TRUE)
#
# #read the shapefile. Alternatively make an assignment, such as f<-sf::read_sf(your_SHP_file)
# sf::read_sf(your_SHP_file)


make_leaflet_map<- function(bound, habras=NULL, traps=NULL, buffer=0, transparency = 1){

	if(!is.null(traps)){
		traps<-st_as_sf(traps, coords = c(1, 2), crs=st_crs(bound))
		traps_buff<-st_buffer(traps, dist=buffer)
		} else {
			traps_buff<-NULL
		}
	if(!is.null(habras)){
			crs(habras)<- st_crs(bound)$wkt #assume same crs as region boundary
			habrasproj<- terra::project(habras, "epsg:4326", method="bilinear")
			nrast<-nlyr(habrasproj)
			}
		else {
			nrast<-0
			habrasproj<-NULL
			}
	if(nrast>0) {
		# Leaflet requires raster object
		if(nrast == 1) habrasproj<- raster::raster(habrasproj)
		else habrasproj<- raster::stack(habrasproj)
	}
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
	m <- m %>%	addPolygons(data=st_transform(bound, 4326), weight=2, fill=FALSE) %>%
		addCircleMarkers(data=st_transform(traps, 4326), color="red", radius=1, group="traps") %>%
		addPolygons(data=st_transform(traps_buff, 4326), color="red", weight=1, group="traps buffer") %>%
		addScaleBar("bottomright", options=scaleBarOptions(imperial=FALSE, maxWidth = 200)) %>%
		addLayersControl(baseGroups=c("OSM (default)", "ESRI Topo", "ESRI Satellite"),
										 overlayGroups = c("traps", "traps buffer", names(habrasproj)),
										 options=layersControlOptions(collapsed=FALSE)) %>%
		hideGroup("traps buffer") %>%
		hideGroup(names(habrasproj)[-1]) #keep the first raster displayed
	return(m)
}
