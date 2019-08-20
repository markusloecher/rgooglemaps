genStaticMap = structure(function#generates a "static map" from map tiles by "stitching" them together
### necssary because the Google static maps API requires a key now
(
  #mt, ##<< list returned by \code{GetMapTiles}
  center, ##<< optional center
  zoom = 15, ##<< zoom
  size = c(640,640), ##<< size (in pixels) of "stitched" map 
  destfile = tempfile("staticMap",fileext = ".png"), ##<<  File to load the map image from or save to, depending on \code{NEWMAP}.
  urlBase = "http://a.tile.openstreetmap.org/", ##<< tileserver URL, alternatives: , "http://mt1.google.com/vt/lyrs=m", "http://tile.stamen.com/toner","http://tile.stamen.com/watercolor" 
  tileDir= "~/mapTiles/OSM/", ##<< map tiles are stored in a local directory, e.g. "~/mapTiles/Google/"
  verbose=0, ##<< level of verbosity
  ... ##<< further arguments to be passed to \code{FUN}
){
  #library(png)
  nTiles = round(size/256)+3
  #browser() 
  mt = GetMapTiles(center=center,zoom=zoom,tileDir = tileDir,
                   urlBase=urlBase, 
                   nTiles = nTiles)
  
  tw = 257/256#257/256 #tile width and height
  X=mt$X;Y=mt$Y
  #rX = range(X);rY = range(Y);
  
  XY=LatLon2XY(center[1],center[2],mt$zoom)
  XY2=as.data.frame(XY$Tile + XY$Coords/256)
  
  
  rX=XY2[,"X"]+ c(-1,1)*size[1]/512
  rY=XY2[,"Y"]+ c(-1,1)*size[2]/512-1
  XY2[,"Y2"] = sum(rY)-XY2[,"Y"]+1
  
  png(destfile,size[1],size[2])
  par(mar=rep(0,4))
  plot(rX+c(0,1/256),rY+c(0,1/256),type="n", axes=FALSE, xlab="", ylab="", asp = 1);
  tmp2 <- par('usr');
  
  updateusr(tmp2[1:2], x2=rX+c(0,1/256), tmp2[3:4], y2=rY+c(0,1/256) );
  usrNew =par("usr")
  if (verbose) cat("user coords:",par("usr"), "\n")
    
  k=0
  for (x in X){
      for (y in Y){
        k=k+1
        if (length(mt$tiles)==0){
          mapFile = file.path(mt$tileDir, paste(mt$zoom, x, y, sep="_"))
          tile=readPNG(paste0(mapFile,mt$tileExt), native=TRUE);
        } else {
          tile = mt$tiles[[k]]
        }
        if (exists("rasterImage")) { # can plot only in R 2.11.0 and higher      
          #if (require(grid)) grid.raster(MyMap[[4]], width=1, height=1, y=0, just="bottom") else 
          yr = sum(rY)-y
          rasterImage(tile, x,yr,x+tw,yr+tw);
          if (verbose>1) rect(x,yr,x+tw,yr+tw)
          if (!all(usrNew ==par("usr"))) #if (verbose)
            cat("placing ", k,"th tile at coords:",x,yr,x+tw,yr+tw, "\n")
        }
      }
  }
  
  
  if (verbose>1) {
    points(XY2[,"X"],XY2[,"Y2"],col=2,pch=20)
    browser()
  }
  dev.off()
  
  
invisible(destfile)
### list with tiles
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



