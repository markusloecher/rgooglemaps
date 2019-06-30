`GetMap` <- structure(function# download a static map from the Google server
### Query the Google server for a static map tile, defined primarily by its 
### center and zoom. Many additional arguments allow the user to customize 
### the map tile.
### documentation at https://developers.google.com/maps/documentation/staticmaps/
(
  center=c(lat=42, lon=-76), ##<< optional center (lat first,lon second  )
  size = c(640,640), ##<< desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels 
  destfile = tempfile("staticMap",fileext = ".png"), ##<<  File to load the map image from or save to, depending on \code{NEWMAP}.
  zoom =12, ##<< Google maps zoom level.
  markers,  ##<< (optional) defines one or more markers to attach to the image at specified locations. This parameter takes a string of marker definitions separated by the pipe character (|) 
  path="",  ##<< (optional) defines a single path of two or more connected points to overlay on the image at specified locations. This parameter takes a string of point definitions separated by the pipe character (|)
  span, ##<< (optional) defines a minimum viewport for the map image expressed as a latitude and longitude pair. The static map service takes this value and produces a map of the proper zoom level to include the entire provided span value from the map`s center point. Note that the resulting map may include larger bounds for either latitude or longitude depending on the rectangular dimensions of the map. If zoom is specified, span is ignored 
  frame, ##<< (optional) specifies that the resulting image should be framed with a colored blue border. The frame consists of a 5 pixel, 55 % opacity blue border. 
  hl, ##<< (optional) defines the language to use for display of labels on map tiles. Note that this paramater is only supported for some country tiles; if the specific language requested is not supported for the tile set, then the default language for that tile set will be used.
  sensor = "true",  ##<< specifies whether the application requesting the static map is using a sensor to determine the user`s location. This parameter is now required for all static map requests.
  maptype = c("roadmap","mobile","satellite","terrain","hybrid","mapmaker-roadmap","mapmaker-hybrid")[2], ##<< defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile. 
  format = c("gif","jpg","jpg-baseline","png8","png32")[5],  ##<< (optional) defines the format of the resulting image. By default, the Static Maps API creates GIF images. There are several possible formats including GIF, JPEG and PNG types. Which format you use depends on how you intend to present the image. JPEG typically provides greater compression, while GIF and PNG provide greater detail. This version supports only PNG.
  extraURL="", ##<< custom URL suffix
  RETURNIMAGE = TRUE, ##<< return image yes/no default: TRUE
  GRAYSCALE =FALSE, ##<< Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY}
  NEWMAP = TRUE, ##<< if TRUE, query the Google server and save to \code{destfile}, if FALSE load from destfile. 
  SCALE = 1, ##<< use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map
  API_console_key, ##<< API key (formerly optional, now mandatory). If missing, the function "stitches" a static map from map tiles
  urlBase = c("http://a.tile.openstreetmap.org/", "http://mt1.google.com/vt/lyrs=m", "http://tile.stamen.com/toner","http://tile.stamen.com/watercolor")[1], ##<< tileserver URL
  tileDir= c("~/mapTiles/OSM/","~/mapTiles/Google/")[1], ##<< map tiles are stored in a local directory
  verbose=0 ##<< level of verbosity
){
  ##note<<Note that size is in order (lon, lat)
  
  if (!NEWMAP) {
    #no new map download
    myMap <- ReadMapTile(destfile); 
    return(myMap);
  }
  
  if (is.character(center)) {
    if (verbose) cat("geocoding ", center, "\n")
    center = getGeoCode(center,verbose)
  }
  if (all(c("lat","lon") %in% names(center))) center = center[c("lat","lon")]
  ##seealso<< \link{GetMap.bbox}
  
#   if (is.null(names(center))) {
#     names(center) = c("lat", "lon");
#   } else stopifnot( all(names(center) %in% c("lat", "lon")) )
#   
  if (!is.null(center)) {
    centerNum = center
    center <- paste(center,collapse=",")
  }
  
  if (!missing(API_console_key)){
    print("API key provided")
    #if (!is.null(API_console_key))  urlStr <- paste0(urlStr,"&key=", API_console_key);
    
    stopifnot(all(size <=640));
   
    if (length(size) < 2) {s <- paste(size,size,sep="x")} else {s <- paste(size,collapse="x");}
    
    # if (missing(format)){	
    #   if ( fileExt == "png") format <- "png32"
    # }
   
    googleurl <- "https://maps.googleapis.com/maps/api/staticmap?"# "http://maps.google.com/maps/api/staticmap?"; # googleurl <- 'http://maps.google.com/staticmap?';
  	
  	if (!missing(span)){#Images may specify a viewport (defined by latitude and longitude values expressed as degrees) to display around a provided center point by passing a span parameter. Defining a minimum viewport in this manner obviates the need to specify an exact zoom level. The static map service uses the span parameter in conjunction with the size parameter to construct a map of the proper zoom level which includes at least the given viewport constraints.
  		span <- paste(span,collapse=",")
  		urlStr <- paste(googleurl, "center=", center, "&span=", span,  "&size=",  s, "&maptype=", maptype, "&format=", format, "&sensor=", sensor, sep="")
  
  	} else 	if (is.null(center) & missing(zoom)) {#let the Static Maps API determine the correct center and zoom level implicitly, based on evaluation of the position of the markers:
  		stopifnot(!missing(markers) | path != "");
  		urlStr <- paste(googleurl,  "size=",  s, "&maptype=", maptype, "&format=", format, "&sensor=", sensor, sep="")
  	} else {
  		stopifnot(!is.null(center), !missing(zoom));
  		urlStr <- paste(googleurl, "center=", center, "&zoom=", zoom,  "&size=",  s, "&maptype=", maptype, "&format=", format, "&sensor=", sensor, sep="")
  	}
  	
  	urlStr <- paste(urlStr, path, sep="");
  	urlStr <- paste(urlStr, extraURL, sep="");
  	
    if (!missing(hl)) urlStr <- paste0(urlStr, "&language=",hl);
    if (SCALE == 2) urlStr <- paste(urlStr, "&scale=", SCALE, sep="");
  	
  	if (!missing(markers)) {
  		#assumes markers is a list with names lat, lon, size (optional), color (optional), char (optional)
  		#if(is.data.frame(markers)) markers<-as.matrix(markers)
  		if ( is.matrix(markers) | is.data.frame(markers)) {
  		  stopifnot(all(c("lat","lon") %in% colnames(markers)))
  		  latlon = which(colnames(markers) %in% c("lat","lon"))
  		  for (i in 1:nrow(markers)){
  		  	m1 <- paste(markers[i,c("lat","lon")], collapse=",");
  		  	if (any(c("size","color","label") %in% colnames(markers) ) ) {
  		  	  m2 <- paste(colnames(markers)[-latlon], markers[i,-latlon], collapse="|",sep=":");
  		  	  m <- paste(m2,m1,sep="|")
  		  	} else {
  		  	  m <- m1
  		  	}
  		  	#m <- paste("&markers=",m,sep="")
  		  	#print(m)
  		  	if (i==1){ 
  		  		markers.string <- paste0("&markers=",m);
  			  } else { 
  				  markers.string <- paste(markers.string,paste0("&markers=",m), sep=""); 
  		    }
  		    #if (verbose) print(markers.string);
  		  }
  		  #browser()
  		  #markers.string <- paste("&markers=", markers.string,sep="")
  		} else if (is.character(markers)) {#already in the correct string format:
  		  markers.string <- markers;
  		} 
  		
  		urlStr <- paste(urlStr, markers.string, sep="");
      
  	}
  
  	if (verbose) print(urlStr);
  	if (is.null(center)) {
  	  if (verbose) print("Note that when center and zoom are not specified, no meta information on the map tile can be stored. This basically means that R cannot compute proper coordinates. You can still download the map tile and view it in R but overlays are not possible.");
  	  #ans <- readLines(n=1);
  	  #if (ans != "y") return(); 
  	  MetaInfo <- list(lat.center = NULL, lon.center  = NULL, zoom = zoom, 
  	                   url = "google", BBOX = NULL, size=size, SCALE = SCALE);
  	  
  	} else if ( is.numeric(centerNum) & !missing(zoom)) {  
  	  myMap <- list(lat.center = centerNum[1], lon.center  = centerNum[2], zoom = zoom, SCALE = SCALE);
  	  BBOX <- list(ll = XY2LatLon(myMap, -size[1]/2 + 0.5, -size[2]/2 - 0.5), ur = XY2LatLon(myMap, size[1]/2 + 0.5, size[2]/2 - 0.5) );
  	  MetaInfo <- list(lat.center = centerNum[1], lon.center  = centerNum[2], zoom = zoom, 
  	                   url = "google", BBOX = BBOX, size=size, SCALE = SCALE);
  	} 
  	if (missing(destfile)) destfile=file.path(tempdir(),"mapTile.png")
  	if (verbose == -1) browser();
  	
  	if (NEWMAP) {
  	  if (!missing(destfile)){ # destfile=file.path(tempdir(),"mapTile.png")
    	  fileBase <- substring(destfile,1, nchar(destfile)-4);
    	  fileExt <-  substring(destfile,nchar(destfile)-2,nchar(destfile));
    	  #save meta information about the image:    
    	  save(MetaInfo, file = paste(destfile,"rda",sep="."));
  	    download.file(urlStr, destfile, mode="wb", quiet = TRUE);
  	    myTile <- readPNG(destfile, native=TRUE);
  	  } else { #do not save to file, read direcly from connection
  	    #o new dependency on curl package: static maps are not saved to file by default any longer, instead read directly from connection
    	  #connectStr=enc2utf8(gsub(' ','%20',"http://maps.google.com/maps/api/staticmap?center=42,-76&zoom=16&size=640x640&maptype=mobile&format=png32&sensor=true")) #Encode URL Parameters
  	    #connectStr=enc2utf8(gsub(' ','%20', urlStr))
  	    #does not work in RStudio, see http://stackoverflow.com/questions/18407177/load-image-from-website !!
    #     con <- curl(urlStr, "rb")
    # 	  binMap=readBin(con,raw(), 1E5)
    # 	  
  	    # 	  myTile <- readPNG(binMap, native=FALSE);
    # 	  close(con)
  	  }
  	} else { #no new map download
  	  myMap <- ReadMapTile(destfile); 
  	}
  } else {# end of missing API key
  
    f=genStaticMap(center=centerNum, destfile = destfile, urlBase=urlBase, tileDir=tileDir, zoom=zoom)
    
    myMap <- list(lat.center = centerNum[1], lon.center  = centerNum[2], zoom = zoom, SCALE = SCALE);
    BBOX <- list(ll = XY2LatLon(myMap, -size[1]/2 + 0.5, -size[2]/2 - 0.5), ur = XY2LatLon(myMap, size[1]/2 + 0.5, size[2]/2 - 0.5) );
    MetaInfo <- list(lat.center = centerNum[1], lon.center  = centerNum[2], zoom = zoom, 
                     url = "google", BBOX = BBOX, size=size, SCALE = SCALE);


    fileBase <- substring(destfile,1, nchar(destfile)-4);
    fileExt <-  substring(destfile,nchar(destfile)-2,nchar(destfile));
    #save meta information about the image:    
    save(MetaInfo, file = paste(destfile,"rda",sep="."));
    myTile <- readPNG(destfile, native=TRUE);
  }  
    
    
	if (GRAYSCALE) {
		#browser()
		myTile <- RGB2GRAY(myTile);
		if (!missing(destfile)) writePNG(myTile, destfile)
	 }

	if (RETURNIMAGE){
 	  #myMap <- ReadMapTile(destfile); 
	  myMap <- list(lat.center= MetaInfo$lat.center, lon.center=MetaInfo$lon.center, zoom=MetaInfo$zoom, 
	                myTile=myTile, BBOX = MetaInfo$BBOX, url = MetaInfo$url, size=size, SCALE=MetaInfo$SCALE);
	  class(myMap) =  "staticMap"
  	return(myMap);
 	}
 	
	invisible(urlStr)
### map structure or URL used to download the tile.
}, ex = function(){
  if (0){#takes too long to run for CRAN check
    
    lat = c(40.702147,40.718217,40.711614);
    lon = c(-74.012318,-74.015794,-73.998284);
    center = c(mean(lat), mean(lon));
    zoom <- min(MaxZoom(range(lat), range(lon)));
    #this overhead is taken care of implicitly by GetMap.bbox(); 
    markers = paste0("&markers=color:blue|label:S|40.702147,-74.015794&markers=color:",
                     "green|label:G|40.711614,-74.012318&markers=color:red|color:red|",
                     "label:C|40.718217,-73.998284")
    myMap <- GetMap(center=center, zoom=zoom,markers=markers);
    #Note that in the presence of markers one often needs to add some extra padding to the 
    #latitude range to accomodate the extent of the top most marker
    
    #add a path, i.e. polyline:
    myMap <- GetMap(center=center, zoom=zoom,
                    path = paste0("&path=color:0x0000ff|weight:5|40.737102,-73.990318|",
                                  "40.749825,-73.987963|40.752946,-73.987384|40.755823,-73.986397"));
    #use implicit geo coding 
    BrooklynMap <- GetMap(center="Brooklyn", zoom=13)
    PlotOnStaticMap(BrooklynMap)
    
    #use implicit geo coding and display labels in Korean:
    BrooklynMap <- GetMap(center="Brooklyn", zoom=13, hl="ko")
    PlotOnStaticMap(BrooklynMap)
    
    #no highways
    ManHatMap <- GetMap(center="Lower Manhattan", zoom=14, 
                        extraURL="&style=feature:road.highway|visibility:off",
                        destfile = "LowerManhattan.png")
    PlotOnStaticMap(ManHatMap)
    
    #reload the map without a new download:
    ManHatMap <- GetMap(destfile = "LowerManhattan.png",NEWMAP=FALSE)
    PlotOnStaticMap(ManHatMap)
  
    #The example below defines a polygonal area within Manhattan, passed a series of 
    #intersections as locations:
    #myMap <- GetMap(path = paste0("&path=color:0x00000000|weight:5|fillcolor:0xFFFF0033|",
    #          "8th+Avenue+%26+34th+St,New+York,NY|8th+Avenue+%26+42nd+St,New+York,NY|",
    #          "Park+Ave+%26+42nd+St,New+York,NY,NY|Park+Ave+%26+34th+St,New+York,NY,NY"),
    #            destfile = "MyTile3a.png");
    
    #note that since the path string is just appended to the URL you can "abuse" the path 
    #argument to pass anything to the query, e.g. the style parameter:
    #The following example displays a map of Brooklyn where local roads have been changed 
    #to bright green and the residential areas have been changed to black:
    # myMap <- GetMap(center="Brooklyn", zoom=12, maptype = "roadmap", 
    #path = paste0("&style=feature:road.local|element:geometry|hue:0x00ff00|",
    #        "saturation:100&style=feature:landscape|element:geometry|lightness:-100"),
    #        sensor='false', destfile = "MyTile4.png",  RETURNIMAGE = FALSE);
    
    #In the last example we set RETURNIMAGE to FALSE which is a useful feature in general
    #if png is not installed. In that cases, the images can still be fetched 
    #and saved but not read into R.
    
    #In the following example we let the Static Maps API determine the correct center and 
    #zoom level implicitly, based on evaluation of the position of the markers. 
    #However, to be of use within R we do need to know the values for zoom and 
    #center explicitly, so it is better practice to compute them ourselves and 
    #pass them as arguments, in which case meta information on the map tile can be saved as well.
    
    #myMap <- GetMap(markers = paste0("&markers=color:blue|label:S|40.702147,-74.015794&",
    #          "markers=color:green|label:G|40.711614,-74.012318&markers=color:red|",
    #          "color:red|label:C|40.718217,-73.998284"), 
    #           destfile = "MyTile1.png",  RETURNIMAGE = FALSE);
  }
})

