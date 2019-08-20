NumTiles <- structure(function#computes the necessary number of tiles from a bounding box and a zoom level
### computes the necessary number of tiles from a bounding box and a zoom level
(  lonR, ##<< longitude range
   latR, ##<< latitude range, 
   zoom =13, ##<< zoom level
   CheckExistingFiles = TRUE, ##<< logical, if TRUE check if files already exist and only download if not!
   tileExt = ".png", ##<< image type of tile
   tileDir= "~/mapTiles/OSM/", ##<< map tiles are stored in a local directory, e.g. "~/mapTiles/Google/"
   verbose=0 ##<< level of verbosity
){
  nTiles=c(0,0)
  if (!missing(lonR) & !missing(latR)) {
    XYmin = LatLon2XY(lat=latR[1], lon=lonR[1],zoom=zoom)
    XYmax = LatLon2XY(lat=latR[2], lon=lonR[2],zoom=zoom)
    nTiles[1] = abs(XYmax$Tile[1,1]-XYmin$Tile[1,1])+1
    nTiles[2] = abs(XYmax$Tile[1,2]-XYmin$Tile[1,2])+1
    center = c(lat=mean(latR),lon=mean(lonR))
  }
  
  if (CheckExistingFiles) {
    XY = LatLon2XY(lat=center["lat"], lon=center["lon"],zoom=zoom)
    tileXY = XY$Tile + as.numeric(XY$Coords > 256)
    
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
    
    fNameServer = function(x,y) paste0(paste(zoom, x, y, sep="_"),tileExt)
    fList = as.vector(outer(X,Y,FUN = fNameServer))
    
    ExistingFiles=list.files(path=tileDir)
    
    fExist = sum(fList %in% ExistingFiles)
    cat("still need to download", prod(nTiles)-fExist,"tiles from ", prod(nTiles), "requested files.\n")
  }
  
  return(nTiles) 
  ### tuple with number of tiles for lon and lat extent
}, ex = function(){
  if (0){
    #US bounding box:
    for (zoom in 4:15) {
      cat("OSM, zoom =", zoom, "\n")
      NumTiles(lonR=c(-135,-66), latR=c(25,54) , zoom=zoom)
    }
    for (zoom in 4:15) {
      cat("Google, zoom =", zoom, "\n")
      NumTiles(lonR=c(-135,-66), latR=c(25,54) , zoom=zoom, tileDir= "~/mapTiles/Google/")
    }
  }
})