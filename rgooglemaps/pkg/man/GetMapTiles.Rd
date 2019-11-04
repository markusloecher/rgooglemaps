\name{GetMapTiles}
\alias{GetMapTiles}
\title{download map tiles from specified map tile servers such as openstreetmap or Google}
\description{Query the server for map tiles, defined uniquely by their 
X and Y ID and zoom. For offline usage, these map tiles are stored in a local directory
Example OSM:http://a.tile.openstreetmap.org/10/549/335.png
Also see https://wiki.openstreetmap.org/wiki/Tile_servers
Example Google mobile: http://mt1.google.com/vt/lyrs=m&x=1325&y=3143&z=13
Example Google satellite: http://mt1.google.com/vt/lyrs=s&x=1325&y=3143&z=13}
\usage{GetMapTiles(center = c(lat = 52.431635, lon = 13.194773), 
    lonR, latR, nTiles = c(3, 3), zoom = 13, type = c("google", 
        "google-m", "google-s", "osm", "osm-hot", "stamen-toner", 
        "stamen-terrain", "stamen-watercolor")[1], 
    urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir = "/tmp/", 
    CheckExistingFiles = TRUE, TotalSleep = NULL, tileExt = ".png", 
    returnTiles = TRUE, verbose = 0)}
\arguments{
  \item{center}{optional center (lat first,lon second  )}
  \item{lonR}{longitude range}
  \item{latR}{latitude range}
  \item{nTiles}{number of tiles in x and y direction}
  \item{zoom}{Google maps zoom level.}
  \item{type}{choice of tile server}
  \item{urlBase}{tileserver URL, alternatives would be "http://a.tile.openstreetmap.org/", "http://tile.stamen.com/toner/","http://tile.stamen.com/watercolor/"}
  \item{tileDir}{map tiles can be stored in a local directory, e.g. "~/mapTiles/Google/"}
  \item{CheckExistingFiles}{logical, if TRUE check if files already exist and only download if not!}
  \item{TotalSleep}{overall time (in seconds) that one is willing to add in between downloads. This is intended to lower the risk of a server denial. If NULL no call to \link{Sys.sleep} is executed}
  \item{tileExt}{image type of tile}
  \item{returnTiles}{return tiles in a list?}
  \item{verbose}{level of verbosity}
}

\value{list with important information}

\author{Markus Loecher}
\note{Note that size is in order (lon, lat)}


\seealso{\link{GetMap.bbox}}
\examples{
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
m = leaflet::leaflet() \%>\% 
  addTiles( urlTemplate = "http:/localhost:8000/mapTiles/OSM/{z}_{x}_{y}.png")
m = leaflet::leaflet() \%>\% 
  addTiles( urlTemplate = "http:/localhost:8000/mapTiles/Google/{z}_{x}_{y}.png")
m = m \%>\% leaflet::setView(-74.01312, 40.71180, zoom = 16)
m = m \%>\% leaflet::addMarkers(-74.01312, 40.71180)

#Quadriga:
m = m \%>\% leaflet::setView(13.39780, 52.51534, zoom = 16)
m = m \%>\% leaflet::addMarkers(13.39780, 52.51534)
}

}
