
# tile2long = function(x,z){ return (x/(2^z)*360-180); }
# tile2lat= function(y,z) { n=pi-2*pi*y/(2^z);
#                           return (180/pi*atan(0.5*(exp(n)-exp(-n)))); }


`GetMapTiles` <- structure(function# download map tiles from specified map tile servers such as openstreetmap or Google
### Query the server for map tiles, defined uniquely by their 
### X and Y ID and zoom. For offline usage, these map tiles are stored in a local directory
### Example OSM:http://a.tile.openstreetmap.org/10/549/335.png
### Also see https://wiki.openstreetmap.org/wiki/Tile_servers
### Example Google mobile: http://mt1.google.com/vt/lyrs=m&x=1325&y=3143&z=13
### Example Google satellite: http://mt1.google.com/vt/lyrs=s&x=1325&y=3143&z=13

(
  center=c(lat=52.431635, lon=13.194773), ##<< optional center (lat first,lon second  )
  lonR, ##<< longitude range
  latR, ##<< latitude range
  nTiles = c(3,3), ##<< number of tiles in x and y direction
  #size = c(640,640), ##<< desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels 
  # taskfile = "Zehlendorf", ##<<  File to save the meta information to.
  zoom =13, ##<< Google maps zoom level.
  # maptype = c("roadmap","mobile","satellite","terrain","hybrid","mapmaker-roadmap","mapmaker-hybrid")[1], ##<< defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile. 
  type = c("google", "google-m","google-s","osm", "osm-hot", "stamen-toner", "stamen-terrain", "stamen-watercolor")[1],  ##<< choice of tile server
  urlBase = "http://mt1.google.com/vt/lyrs=m" , ##<< tileserver URL, alternatives would be "http://a.tile.openstreetmap.org/", "http://tile.stamen.com/toner/","http://tile.stamen.com/watercolor/"
  tileDir= "/tmp/", ##<< map tiles can be stored in a local directory, e.g. "~/mapTiles/Google/"
  CheckExistingFiles = TRUE, ##<< logical, if TRUE check if files already exist and only download if not!
  TotalSleep = NULL, ##<< overall time (in seconds) that one is willing to add in between downloads. This is intended to lower the risk of a server denial. If NULL no call to \link{Sys.sleep} is executed
  #format = c("gif","jpg","jpg-baseline","png8","png32")[5],  ##<< (optional) defines the format of the resulting image. By default, the Static Maps API creates GIF images. There are several possible formats including GIF, JPEG and PNG types. Which format you use depends on how you intend to present the image. JPEG typically provides greater compression, while GIF and PNG provide greater detail. This version supports only PNG.
  #RETURNIMAGE = TRUE, ##<< return image yes/no default: TRUE
  #GRAYSCALE =FALSE, ##<< Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY}
  #NEWMAP = TRUE, ##<< if TRUE, query the Google server and save to \code{destfile}, if FALSE load from destfile. 
  #SCALE = 1, ##<< use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map
  tileExt = ".png", ##<< image type of tile
  returnTiles = TRUE, ##<< return tiles in a list?
  verbose=0 ##<< level of verbosity
){
  ##note<<Note that size is in order (lon, lat)
  #if (missing(destfile)) destfile=file.path(tempdir(),"mapTile.png")
  #require(RgoogleMaps)
  
  type = switch(type,
         "google" = "google-m",
         "stamen" = "stamen-toner",
         type
  )
  tileservers = c("osm"= "http://a.tile.openstreetmap.org/",
                  "osm-hot"= "http://a.tile.openstreetmap.fr/hot/",
                  "google-m" = "http://mt1.google.com/vt/lyrs=m",
                  "google-s" = "http://mt1.google.com/vt/lyrs=s",
                  "stamen-toner" = "http://tile.stamen.com/toner/",
                  "stamen-watercolor" ="http://tile.stamen.com/watercolor/",
                  "stamen-terrain" = "http://tile.stamen.com/terrain/",
                  "osm-public-transport"= "http://tile.memomaps.de/tilegen/"
                  )
  
  if (type %in% names(tileservers)) urlBase=tileservers[type]
  
  if (verbose) cat("tileserver:", urlBase, "\n")
  
  if (is.character(center)) {
    if (verbose) cat("geocoding ", center, "\n")
    center = getGeoCode(center,verbose)
    if (verbose) cat("result:", center, "\n")
  }
  if (all(c("lat","lon") %in% names(center))) center = center[c("lat","lon")] else names(center) = c("lat","lon")
  ##seealso<< \link{GetMap.bbox}
  #turn out we do NOT HAVE TO cap the zoom to a fake 13 after all !!
  # We can truthfully record the correct zoom level, YES
  fakeZoom = zoom#min(zoom,13)
  #if (!is.null(center)) center <- paste(center,collapse=",")
  if (!missing(lonR) & !missing(latR)) {
    XYmin = LatLon2XY(lat=latR[1], lon=lonR[1],zoom=zoom)
    XYmax = LatLon2XY(lat=latR[2], lon=lonR[2],zoom=zoom)
    nTiles[1] = abs(XYmax$Tile[1,1]-XYmin$Tile[1,1])+1
    nTiles[2] = abs(XYmax$Tile[1,2]-XYmin$Tile[1,2])+1
    #browser()
    #if (missing(center) | is.null(center)) 
    center = c(lat=mean(latR),lon=mean(lonR))  
    if (verbose){
      cat("nTiles=",nTiles,", center=", round(center,3), "\n")
    }
  } #else {
    XY = LatLon2XY(lat=center["lat"], lon=center["lon"],zoom=zoom)
    tileXY = XY$Tile + as.numeric(XY$Coords > 256)
  #}
  if (nTiles[1] %% 2 == 0) {#even
    X = (tileXY[1,1]-nTiles[1]/2):(tileXY[1,1]+nTiles[1]/2-1);
  } else {
    X = (tileXY[1,1]-(nTiles[1]-1)/2):(tileXY[1,1]+(nTiles[1]-1)/2);
  }
  if (nTiles[2] %% 2 == 0) {#even
    Y = (tileXY[1,2]-nTiles[2]/2):(tileXY[1,2]+nTiles[2]/2-1);
  } else {
    Y = (tileXY[1,2]-(nTiles[2]-1)/2):(tileXY[1,2]+(nTiles[2]-1)/2);
  }
  
  if (verbose>1) browser() 
  if (isTRUE(tileDir)) tileDir = paste0("~/mapTiles/",type,"/")
    
  if (!is.null(tileDir)) {
    if (!dir.exists(tileDir)) {
      if (verbose) cat("trying to create dir",tileDir, "\n")
      dir.create(tileDir, recursive = TRUE)
    }
    if (CheckExistingFiles) ExistingFiles=list.files(path=tileDir)
  }
  NumTiles = length(X)*length(Y)
  if (verbose) cat (NumTiles, "tiles to download \n")
    
  #http://a.tile.openstreetmap.org/15/9647/12321.png
  DOWNLOAD=TRUE;sleptTotal=0
  
  if (grepl("watercolor",urlBase) | grepl("terrain",urlBase) | grepl("lyrs=s|lyrs=y",urlBase)){
    tileExt = ".jpg"
  }
  if (verbose) cat("tile image format:", tileExt, "\n")
    
  k=1;tiles=list()
  for (x in X){
    for (y in Y){
      if (grepl("openstreetmap|stamen",urlBase) | grepl("osm",type)){
        if (grepl("watercolor",urlBase) | grepl("terrain",urlBase)){
          tileExt = ".jpg"
          url <- paste0(urlBase, zoom, "/",x , "/", y, ".jpg")#not necessary as the stamenWeb server automatically converts the png request to jpg
        } else {
          url <- paste0(urlBase, zoom, "/",x , "/", y, ".png")
        }
        
      } else if (grepl("google",urlBase)){
        url <- paste0(urlBase, "&x=", x, "&y=", y, "&z=", zoom)
        if (grepl("lyrs=s|lyrs=y",urlBase)) tileExt = ".jpg" #satellite or hybrid
      } 
      
		  #browser()
      #print(url)
      # we need to keep the x and y coords to 4 digits!
      #xFake = x
      #yFake = y
      f=paste(zoom, x, y, sep="_")
		  if (CheckExistingFiles) 
        if (paste0(f,tileExt) %in% ExistingFiles) {
          if (verbose) cat("NOT downloading existing file ",f, tileExt, "\n",sep="" )
          DOWNLOAD=FALSE
        } else {
          DOWNLOAD=TRUE
        }
      destfile = file.path(tileDir, f)
      mapFile=paste0(destfile,tileExt)
      if (DOWNLOAD){
  		  if (!is.null(TotalSleep)){
  		    sleep_a_bit = round(runif(1,max=2*TotalSleep/NumTiles),1)
  		    Sys.sleep(sleep_a_bit)
  		    sleptTotal= sleptTotal+sleep_a_bit
  		  }
        res=try(download.file(url, mapFile, mode="wb", quiet = TRUE));
        if (class(res)=="try-error"){
          tmp=RCurl::getBinaryURL(url)
          if (tileExt == ".png") png::writePNG(tmp, mapFile)
        }
      }
      
      if (tileExt == ".jpg") {
        readImg = jpeg::readJPEG
      } else if (tileExt == ".png")  {
        readImg = png::readPNG
      }
      if (returnTiles){
        res=try(readImg(mapFile, native=TRUE))
        if (class(res)=="try-error"){#download again
          download.file(url, mapFile, mode="wb", quiet = TRUE);
        } else tiles[[k]]=res
        
      } 
      #browser()
		  k=k+1
    }
  }
  cat("sleptTotal=",sleptTotal, "\n")
  mt = list(X=X,Y=Y,zoom=zoom,tileDir=tileDir,tileExt=tileExt,type=type, tiles=tiles)
  class(mt) =  "mapTiles"
  invisible(mt)	
### list with important information
}, ex = function(){
  if (0){
    
    #OSM, Ireland
    xlim = c(-7, -3.5) 
    ylim = c(51.35, 55.35)
    Dublin = c(lon=-6.266155,lat=53.350140)
    DublinMerc = geosphere_mercator(Dublin)
    
    ir.osm <- GetMapTiles(lonR=xlim, latR=ylim, zoom=7, verbose=1,
                          type = "osm", tileDir= TRUE)
    map = plotOSM(ir.osm)
    par("usr")#A vector of the form c(x1, x2, y1, y2)
    points(map$bbox$upperLeft,col=2,pch=20)
    points(map$bbox$lowerRight,col=2,pch=20)
    
    points(DublinMerc, col =2, pch=1,cex=1.5)
    
    ir.stamenToner <- GetMapTiles(lonR=xlim, latR=ylim, zoom=7,verbose=0,
                                  type = "stamen", tileDir= TRUE)
    plotOSM(ir.stamenToner)
    
    ir.stamenWater <- GetMapTiles(lonR=xlim, latR=ylim, zoom=7, verbose=1,
                                  type = "stamen-watercolor", tileDir= TRUE)
    plotOSM(ir.stamenWater)
   
    
    #############################################
    zoom=5
    nTiles = prod(NumTiles(lonR=c(-135,-66), latR=c(25,54) , zoom=zoom))
    us_google_5 = GetMapTiles(lonR=c(-135,-66), latR=c(25,54) , zoom=zoom, TotalSleep = 2*nTiles,
                type = "google", tileDir= TRUE, verbose = TRUE)
    
    PlotOnMapTiles(us_google_5)
    wtc_ll = getGeoCode("World Trade Center, NY")
    wtc_google_15=GetMapTiles(wtc_ll, zoom=15,nTiles = c(3,3), type = "google", 
                              tileDir= TRUE, verbose = 1)
    PlotOnMapTiles(wtc_google_15)
    wtc_google_16 =GetMapTiles(wtc_ll, zoom=16,nTiles = c(4,4), type = "google", 
                               tileDir= TRUE, verbose=1)
    PlotOnMapTiles(wtc_google_16)

  
    wtc_stamen=GetMapTiles(wtc_ll, zoom=15,nTiles = c(3,3), verbose=1,
                    type = "stamen-toner", tileDir= TRUE)
    PlotOnMapTiles(wtc_stamen)
  

  
  ###combine with leaflet:
  #From:http://stackoverflow.com/questions/5050851/
  #     best-lightweight-web-server-only-static-content-for-windows
  #To use Python as a simple web server just change your working 
  #directory to the folder with your static content and type 
  #python -m SimpleHTTPServer 8000, everything in the directory
  #will be available at http:/localhost:8000/
    
  library(leaflet)
  m = leaflet::leaflet() %>% 
    addTiles( urlTemplate = "http:/localhost:8000/mapTiles/OSM/{z}_{x}_{y}.png")
  m = leaflet::leaflet() %>% 
    addTiles( urlTemplate = "http:/localhost:8000/mapTiles/Google/{z}_{x}_{y}.png")
  m = m %>% leaflet::setView(-74.01312, 40.71180, zoom = 16)
  m = m %>% leaflet::addMarkers(-74.01312, 40.71180)
  
  #Quadriga:
  m = m %>% leaflet::setView(13.39780, 52.51534, zoom = 16)
  m = m %>% leaflet::addMarkers(13.39780, 52.51534)
  }
  
}) 


