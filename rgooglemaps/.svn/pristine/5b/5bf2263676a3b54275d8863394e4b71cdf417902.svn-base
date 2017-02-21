\name{GetMapTiles}
\alias{GetMapTiles}
\title{download map tiles from specified map tile servers such as openstreetmap or Google}
\description{Query the server for map tiles, defined uniquely by their 
X and Y ID and zoom. For offline usage, these map tiles are stored in a local directory}
\usage{GetMapTiles(center = c(lat = 52.431635, lon = 13.194773), lonR, 
    latR, nTiles = c(3, 3), zoom = 13, urlBase = c("http://a.tile.openstreetmap.org/", 
        "http://mt1.google.com/vt/lyrs=m")[1], CheckExistingFiles = TRUE, 
    TotalSleep = NULL, tileExt = ".png", tileDir = "~/mapTiles/OSM/", 
    returnTiles = FALSE, verbose = 0)}
\arguments{
  \item{center}{optional center (lat first,lon second  )}
  \item{lonR}{longitude range}
  \item{latR}{latitude range}
  \item{nTiles}{number of tiles in x and y direction}
  \item{zoom}{Google maps zoom level.}
  \item{urlBase}{tileserver URL}
  \item{CheckExistingFiles}{logical, if TRUE check if files already exist and only download if not!}
  \item{TotalSleep}{overall time (in seconds) that one is willing to add in between downloads. This is intended to lower the risk of a server denial. If NULL no call to \link{Sys.sleep} is executed}
  \item{tileExt}{image type of tile}
  \item{tileDir}{map tiles are stored in a local directory}
  \item{returnTiles}{return tiles in a list?}
  \item{verbose}{level of verbosity}
}

\value{list with important information}

\author{Markus Loecher}
\note{Note that size is in order (lon, lat)}


\seealso{\link{GetMap.bbox}}
\examples{
if (0){
tmp=GetMapTiles("World Trade Center, NY", zoom=15,nTiles = c(5,5), verbose=1)
PlotOnMapTiles(tmp)
tmp=GetMapTiles("World Trade Center, NY", zoom=16,nTiles = c(20,20), verbose=1)

tmp2=GetMapTiles("World Trade Center, NY", zoom=15,nTiles = c(5,5), verbose=1,
                urlBase = "http://mt1.google.com/vt/lyrs=m", 
                tileDir= "~/mapTiles/Google/")
tmp=GetMapTiles("Hoboken, NJ", zoom=16,nTiles = c(30,30), verbose=1,
                urlBase = "http://mt1.google.com/vt/lyrs=m", 
                tileDir= "~/mapTiles/Google/")
PlotOnMapTiles(tmp2)

tmp2=GetMapTiles("Werderscher Markt 15, 10117 Berlin", zoom=15,nTiles = c(20,20), verbose=0,
                 urlBase = "http://mt1.google.com/vt/lyrs=m", 
                 tileDir= "~/mapTiles/Google/")
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
