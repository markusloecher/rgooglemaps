\name{genStaticMap}
\alias{genStaticMap}
\title{generates a "static map" from map tiles by "stitching" them together}
\description{necssary because the Google static maps API requires a key now}
\usage{genStaticMap(center, zoom = 15, size = c(640, 640), 
    destfile = tempfile("staticMap", fileext = ".png"), 
    type = c("google", "google-m", "google-s", "osm", 
        "osm-hot", "stamen-toner", "stamen-terrain", 
        "stamen-watercolor")[1], urlBase = "http://mt1.google.com/vt/lyrs=m", 
    tileDir = "/tmp/", tileExt = ".png", verbose = 0, 
    ...)}
\arguments{
  \item{center}{optional center}
  \item{zoom}{zoom}
  \item{size}{size (in pixels) of "stitched" map}
  \item{destfile}{File to load the map image from or save to, depending on \code{NEWMAP}.}
  \item{type}{choice of tile server}
  \item{urlBase}{tileserver URL, alternatives would be "http://a.tile.openstreetmap.org/", "http://tile.stamen.com/toner/","http://tile.stamen.com/watercolor/"}
  \item{tileDir}{map tiles can be stored in a local directory, e.g. "~/mapTiles/Google/"}
  \item{tileExt}{image type of tile}
  \item{verbose}{level of verbosity}
  \item{\dots}{further arguments to be passed to \code{FUN}}
}

\value{list with tiles}

\author{Markus Loecher}




\examples{

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
}
