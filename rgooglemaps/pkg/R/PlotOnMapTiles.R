PlotOnMapTiles = structure(function#plots on map tiles by "stitching" them together
### Counterpart to \code{PlotOnStaticMap} for map tiles
(
  mt, ##<< list returned by \code{GetMapTiles}
  lat, ##<< latitude values to be overlaid, if any
  lon, ##<< longitude values to be overlaid, if any
  center, ##<< optional center
  size = c(768,768), ##<< size (in pixels) of "stitched" map 
  add=FALSE, ##<< start a new plot or add to an existing
  FUN = points, ##<< plotting function to use for overlay; typical choices would be \link{points} and \link{lines} 
  mar=c(0,0,0,0), ##<< outer margin in plot; if you want to see axes, change the default
  verbose=0, ##<< level of verbosity
  ... ##<< further arguments to be passed to \code{FUN}
){
  #library(png)
  if (mt$tileExt == ".jpg") {
    readImg = jpeg::readJPEG
  } else if (mt$tileExt == ".png")  {
    readImg = png::readPNG
  }
  par(mar=mar)
  tw = 257/256#257/256 #tile width and height
  X=mt$X;Y=mt$Y
  rX = range(X);rY = range(Y);
  if (0) if (missing(center)) {
    if (!missing(lat) & !missing(lon)){
      centerXY=LatLon2XY(mean(lat),mean(lon),mt$zoom)
      XY2=as.data.frame(centerXY$Tile + centerXY$Coords/256)
      XY2[,"Y2"] = sum(rY)-XY2[,"Y"]
      xx=XY2[,"X"]+size[1]/512*c(-1,1)
      yy=XY2[,"Y2"]+size[2]/512*c(-1,1)
      plot(xx,yy,type="n", axes=FALSE, xlab="", ylab="", asp = 1);
      tmp2 <- par('usr');
      updateusr(tmp2[1:2], x2=xx, tmp2[3:4], y2=yy );
    } else {
      
    }
  }
  if (!add){
    plot(rX+c(0,tw),rY+c(0,tw),type="n", axes=FALSE, xlab="", ylab="", asp = 1);
    tmp2 <- par('usr');
    updateusr(tmp2[1:2], x2=rX+c(0,tw), tmp2[3:4], y2=rY+c(0,tw) );
    if (verbose) cat("user coords:",par("usr"), "\n")
    
    k=1
    for (x in X){
      for (y in Y){
        if (length(mt$tiles)==0){
          mapFile = file.path(mt$tileDir, paste(mt$zoom, x, y, sep="_"))
          tile=readImg(paste0(mapFile,mt$tileExt), native=TRUE);
        } else {
          tile = mt$tiles[[k]]
          k=k+1
        }
        if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher      
          #if (require(grid)) grid.raster(MyMap[[4]], width=1, height=1, y=0, just="bottom") else 
          yr = sum(rY)-y
          rasterImage(tile, x,yr,x+tw,yr+tw);
          if (verbose) cat("placing tile at coords:",x,yr,x+tw,yr+tw, "\n")
        }
      }
    }
  }
  
  if (!missing(lat) & !missing(lon)){
    stopifnot(length(lat)==length(lon))
    XY=LatLon2XY(lat,lon,mt$zoom)
    XY2=as.data.frame(XY$Tile + XY$Coords/256)
    #browser()
    XY2[,"Y2"] = sum(rY)-XY2[,"Y"]
    if (verbose) {
      cat("lat lon become:")
      print(XY2)
    }
    FUN(XY2[,"X"],XY2[,"Y2"]+1,...)
  }
### nothing returned
}, ex = function(){

  if (0){
    lat = c(40.702147,40.718217,40.711614);
    lon = c(-74.012318,-74.015794,-73.998284);
    center = c(mean(lat), mean(lon));
    zoom <- min(MaxZoom(range(lat), range(lon)));
    bb=qbbox(lat,lon)
    
    mt = GetMapTiles(latR =bb$latR , lonR=bb$lonR,zoom=zoom,verbose=1)
    PlotOnMapTiles(mt,lat=lat,lon=lon,pch=20,col=c('red', 'blue', 'green'),cex=2)
    
    mt = GetMapTiles(latR =bb$latR , lonR=bb$lonR,zoom=zoom,
                     tileDir= "~/mapTiles/Google/")
    PlotOnMapTiles(mt,lat=lat,lon=lon,pch=20,col=c('red', 'blue', 'green'),cex=2)
    
  }
})
