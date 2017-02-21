`plotmap` <-structure(function# easy to use wrapper function
###note the similarity in name to PBSmapping::plotMap
###This function is the workhorse of the package RgoogleMaps. It overlays plot on background image of map tile.

(
  lat, ##<< latitude values to be overlaid OR string to be geocoded!
  lon, ##<< longitude values to be overlaid
  map, ##<< optional map object 
  zoom = NULL, ##<< Google maps zoom level
  API = c("google","OSM","bing", "google2")[1], ##<< choice  of map tile API
  maptype = c("roadmap","mobile","satellite","terrain","hybrid","mapmaker-roadmap","mapmaker-hybrid")[2], ##<< defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile. 
  destfile, ##<<  File to save the map image to
  data, ##<< data to look up variables in
  alpha = 1, ##<< opacity
  col = 1, ##<< plot color
  apiKey = NULL, ##<< optional API key (allows for higher rate of downloads for Google); mandatory for Bing maps
  verbose = 0, ##<< level of verbosity 
  ... ##<< further arguments to be passed to \code{PlotOnStaticMap}
){
  
  # argnames <- names(as.list(match.call(expand.dots = FALSE)[-1]))
  # arguments <- as.list(match.call()[-1])
  # env <- parent.frame()
  # args <- as.list(match.call(expand.dots = TRUE)[-1])
  # argsgiven <- names(args)
  #browser()
  # 
  # if ("col" %in% argsgiven) 
  #   col <- eval(args$col)
  
  
  if (!missing(data)) {
    lon = data[, deparse(substitute(lon))]
    lat = data[, deparse(substitute(lat))]
    #for (v in 1:length(data)) assign(names(data)[v], data[[v]])
    # browser()
    # lat=with(incidents,lat)
    # lon=with(incidents,lon)
    #col=with(incidents,col)
    if (is.character(col)) col=data[,col]
    #
    #attach(data, -1, warn.conflicts = FALSE) # bad idea!
  }
  if (missing(destfile)) destfile = tempfile()
      
  if (is.character(lat) & missing(map)){
    if (API == "google") {
      map = GetMap(getGeoCode(lat), zoom = zoom,maptype=maptype,destfile=destfile)
    } else if (API == "OSM") {
      map = GetOsmMap(getGeoCode(lat), zoom = zoom,maptype=maptype,destfile=destfile)
    } else if (API == "bing") {
      map = GetBingMap(getGeoCode(lat), zoom = zoom, maptype=maptype, destfile=destfile,apiKey=apiKey)
    } 
    #browser()
    PlotOnStaticMap(MyMap=map)
    invisible(map)
    return(map)
  }    
  
  if (is.numeric(lat) & is.numeric(lon) ){
    bb=qbbox(lat,lon)
    if (missing(map)){
      if (missing(zoom)){
        zoom <- min(MaxZoom(bb$latR, bb$lonR, c(720,720)))
        if (API == "google") {
          map = GetMap.bbox(bb$lonR, bb$latR, maptype=maptype,destfile=destfile)
        } else if (API == "OSM") {
          #map = GetOsmMap(lonR= bb$lonR, latR =bb$latR, zoom = zoom,maptype=maptype,destfile=destfile)
          map = GetMapTiles(lonR=bb$lonR, latR=bb$latR,zoom=zoom, verbose=verbose)
          #browser()
        } else if (API == "google2") {
          #map = GetOsmMap(lonR= bb$lonR, latR =bb$latR, zoom = zoom,maptype=maptype,destfile=destfile)
          map = GetMapTiles(lonR=bb$lonR, latR=bb$latR,zoom=zoom, verbose=verbose,
                            urlBase = "http://mt1.google.com/vt/lyrs=m", 
                            tileDir= "~/mapTiles/Google/")
          #browser()
        } else if (API == "bing") {
          bbM=do.call("cbind",bb);
          ll= bbM[1,] #lower left corner
          ur= bbM[2,] #upper right corner 
          map = GetBingMap(mapArea=c(ll,ur), zoom = zoom, maptype=maptype, destfile=destfile,apiKey=apiKey)
        } 
        
      } else {
        center=c(lat=mean(lat,na.rm=TRUE),lon=mean(lon,na.rm=TRUE))
        if (API == "google") {
          map = GetMap(center=center, zoom = zoom,maptype=maptype,destfile=destfile)
        } else if (API == "OSM") {
          #map = GetOsmMap(center=center, zoom = zoom,maptype=maptype,destfile=destfile)
          map = GetMapTiles(lonR=bb$lonR, latR=bb$latR,zoom=zoom, verbose=verbose)
          #browser()
        } else if (API == "google2") {
          #map = GetOsmMap(lonR= bb$lonR, latR =bb$latR, zoom = zoom,maptype=maptype,destfile=destfile)
          map = GetMapTiles(lonR=bb$lonR, latR=bb$latR,zoom=zoom, verbose=verbose,
                            urlBase = "http://mt1.google.com/vt/lyrs=m", 
                            tileDir= "~/mapTiles/Google/")
          #browser()
        } else if (API == "bing") {
          map = GetBingMap(center=center, zoom = zoom, maptype=maptype, destfile=destfile,apiKey=apiKey)
        } 

      }
    } 
    if (verbose>1) browser()
    plotclr = col
    if (alpha < 1 & !all(is.na(as.numeric(col)))) plotclr = AddAlpha(as.numeric(col), alpha = alpha, verbose = 0)
    if (class(map) ==  "mapTiles") {
      PlotOnMapTiles(map,lat=lat,lon=lon,col=plotclr, ...)
    } else {#if (class(map) ==  "staticMap") {
      PlotOnStaticMap(MyMap=map, lat=lat,lon=lon, col=plotclr, ...)
    }
    invisible(map)
  }
}, ex = function(){
  if (0){
    #####################Google maps#############################
    mapBG1 = plotmap("Brandenburg Gate, Berlin", zoom = 15)
  
    #####################bing maps#############################
    
    #for bing maps you will need your own API key, 
    #sign up at https://msdn.microsoft.com/en-us/library/ff428642.aspx
    apiKey = scan("C:/Users/loecherm/Dropbox/stuff/bingAPIkey.txt",what="")
    mapBG2 = plotmap("Brandenburg Gate, Berlin", zoom = 15, API = "bing", apiKey=apiKey)
    
    latlon <- cbind.data.frame(lat = c(38.898648,38.889112, 38.880940), 
                                lon = c(-77.037692, -77.050273, -77.03660));
    
    
    map3 = plotmap(lat = latlon$lat, lon = latlon$lon, API = "bing", apiKey=apiKey,
                   col = "purple", pch="X",cex=1.5)
    
    
    #####################OSM maps#############################
    map4 = plotmap(lat = latlon$lat, lon = latlon$lon, API = "OSM", zoom=15,
                   col = "purple", pch="X",cex=1.5)
  }
})
