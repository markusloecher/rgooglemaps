\name{addPolysToMapTiles}
\alias{addPolysToMapTiles}
\title{overlays polygons on map constructed from tiles}
\description{This function plots/overlays polygons on a map tile composition. Typically, the polygons originate from a shapefile.}
\usage{addPolysToMapTiles(mt, polys, col, border = NULL, lwd = 0.25, 
    verbose = 0, add = TRUE, textInPolys = NULL, ...)}
\arguments{
  \item{mt}{list returned by \code{GetMapTiles}}
  \item{polys}{or of class \link[sp]{SpatialPolygons} from the package sp
polygons to overlay; these can be either of class \link[PBSmapping]{PolySet} from the package PBSmapping}
  \item{col}{(optional) vector of colors, one for each polygon}
  \item{border}{the color to draw the border. The default, NULL, means to use \link{par}("fg"). Use border = NA to omit borders, see \link{polygon}}
  \item{lwd}{line width, see \link{par}}
  \item{verbose}{level of verbosity}
  \item{add}{start a new plot or add to an existing}
  \item{textInPolys}{text to be displayed inside polygons.}
  \item{\dots}{further arguments passed to \code{PlotOnStaticMap}}
}



\author{Markus Loecher}



\seealso{\link{PlotOnStaticMap} \link{mypolygon}}
\examples{
if (0){
  #require(PBSmapping);
  shpFile <- paste(system.file(package = "RgoogleMaps"), "/shapes/bg11_d00.shp", sep = "")
  #shpFile <- system.file('bg11_d00.shp', package = "RgoogleMaps");
  library(PBSmapping)
  shp=importShapefile(shpFile,projection="LL");
  bb <- qbbox(lat = shp[,"Y"], lon = shp[,"X"],margin = list(m = rep(50,4), TYPE = "perc"));
  
  mt = GetMapTiles(latR =bb$latR , lonR=bb$lonR,zoom=12,
                   tileDir = "~/mapTiles/Google/",
                   urlBase="http://mt1.google.com/vt/lyrs=m")
  
  PlotOnMapTiles(mt)
  addPolysToMapTiles(mt,shp,lwd=.5, col = rgb(0.25,0.25,0.25,0.025))
  
  #open street mapa:
  mt = GetMapTiles(latR =bb$latR , lonR=bb$lonR,zoom=12)
  
  PlotOnMapTiles(mt)
  addPolysToMapTiles(mt,shp,lwd=.5, col = rgb(0.25,0.25,0.25,0.025))
  #North Carolina SIDS data set:
  shpFile <- system.file("shapes/sids.shp", package="maptools");
  shp=importShapefile(shpFile,projection="LL");
  bb <- qbbox(lat = shp[,"Y"], lon = shp[,"X"]);
  
  #compute regularized SID rate
  sid <- 100*attr(shp, "PolyData")$SID74/(attr(shp, "PolyData")$BIR74+500)
  b <- as.integer(cut(sid, quantile(sid, seq(0,1,length=8)) ));
  b[is.na(b)] <- 1;
  opal <- col2rgb(grey.colors(7), alpha=TRUE)/255; opal["alpha",] <- 0.2;
  shp[,"col"] <- rgb(0.1,0.1,0.1,0.2);
  for (i in 1:length(b)) 
    shp[shp[,"PID"] == i,"col"] <- rgb(opal[1,b[i]],opal[2,b[i]],opal[3,b[i]],opal[4,b[i]]);
  
  mt = GetMapTiles(latR =bb$latR , lonR=bb$lonR,zoom=12,
                   tileDir = "~/mapTiles/Google/",
                   urlBase="http://mt1.google.com/vt/lyrs=m")
  
  PlotOnMapTiles(mt)
  addPolysToMapTiles(mt,shp,lwd=.5, col = rgb(0.25,0.25,0.25,0.025))
  
  
  PlotPolysOnStaticMap(MyMap, shp, lwd=.5, col = shp[,"col"], add = F);
  
  
 }
}
