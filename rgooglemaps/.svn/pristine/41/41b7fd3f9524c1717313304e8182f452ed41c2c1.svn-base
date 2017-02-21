\name{PlotPolysOnStaticMap}
\alias{PlotPolysOnStaticMap}
\title{plots polygons on map}
\description{This function plots/overlays polygons on a map. Typically, the polygons originate from a shapefile.}
\usage{PlotPolysOnStaticMap(MyMap, polys, col, border = NULL, lwd = 0.25, 
    verbose = 0, add = TRUE, textInPolys = NULL, ...)}
\arguments{
  \item{MyMap}{map image returned from e.g. \code{GetMap()}}
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
if (interactive()){
  #require(PBSmapping);
  shpFile <- paste(system.file(package = "RgoogleMaps"), "/shapes/bg11_d00.shp", sep = "")
  #shpFile <- system.file('bg11_d00.shp', package = "RgoogleMaps");
  
  shp=importShapefile(shpFile,projection="LL");
  bb <- qbbox(lat = shp[,"Y"], lon = shp[,"X"]);
  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "DC.png");
  PlotPolysOnStaticMap(MyMap, shp, lwd=.5, col = rgb(0.25,0.25,0.25,0.025), add = F);
  
  #Try an open street map:

  mapOSM <- GetMap.OSM(lonR=bb$lonR, latR=bb$latR, scale = 150000, destfile = "DC.png");
  PlotPolysOnStaticMap(mapOSM, shp, lwd=.5, col = rgb(0.75,0.25,0.25,0.15), add = F);

  #North Carolina SIDS data set:
  shpFile <- system.file("shapes/sids.shp", package="maptools");
  shp=importShapefile(shpFile,projection="LL");
  bb <- qbbox(lat = shp[,"Y"], lon = shp[,"X"]);
  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "SIDS.png");
  #compute regularized SID rate
  sid <- 100*attr(shp, "PolyData")$SID74/(attr(shp, "PolyData")$BIR74+500)
  b <- as.integer(cut(sid, quantile(sid, seq(0,1,length=8)) ));
  b[is.na(b)] <- 1;
  opal <- col2rgb(grey.colors(7), alpha=TRUE)/255; opal["alpha",] <- 0.2;
  shp[,"col"] <- rgb(0.1,0.1,0.1,0.2);
  for (i in 1:length(b)) 
    shp[shp[,"PID"] == i,"col"] <- rgb(opal[1,b[i]],opal[2,b[i]],opal[3,b[i]],opal[4,b[i]]);
  PlotPolysOnStaticMap(MyMap, shp, lwd=.5, col = shp[,"col"], add = F);
  
  #compare the accuracy of this plot to a Google Map overlay:
  library(maptools);
  qk <- SpatialPointsDataFrame(as.data.frame(shp[, c("X","Y")]), as.data.frame(shp[, c("X","Y")]))
  sp::proj4string(qk) <- CRS("+proj=longlat");
  tf <- "NC.counties";
  SGqk <- GE_SpatialGrid(qk)
  png(file=paste(tf, ".png", sep=""), width=SGqk$width, height=SGqk$height,
  bg="transparent")
  par(mar=c(0,0,0,0), xaxs="i", yaxs="i");par(mai = rep(0,4))
  PBSmapping::plotPolys(shp, plt=NULL)
  dev.off()
  maptools::kmlOverlay(SGqk, paste(tf, ".kml", sep=""), paste(tf, ".png", sep=""));
  #This kml file can now be inspected in Google Earth or Google Maps
  
  #or choose an aspect ratio that corresponds better to North Carolina's elongated shape:
  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "SIDS.png", size = c(640, 320), zoom = 7);
  PlotPolysOnStaticMap(MyMap, shp, lwd=.5, col = shp[,"col"], add = F);
 }
}
