\name{plotmap}
\alias{plotmap}
\title{easy to use wrapper function}
\description{note the similarity in name to PBSmapping::plotMap
This function is the workhorse of the package RgoogleMaps. It overlays plot on background image of map tile.}
\usage{plotmap(lat, lon, map, zoom = NULL, API = c("google", "OSM", 
    "bing", "google2")[1], maptype = c("roadmap", "mobile", "satellite", 
    "terrain", "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")[2], 
    destfile, data, alpha = 1, col = 1, apiKey = NULL, verbose = 0, 
    ...)}
\arguments{
  \item{lat}{latitude values to be overlaid OR string to be geocoded!}
  \item{lon}{longitude values to be overlaid}
  \item{map}{optional map object}
  \item{zoom}{Google maps zoom level}
  \item{API}{choice  of map tile API}
  \item{maptype}{defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile.}
  \item{destfile}{File to save the map image to}
  \item{data}{data to look up variables in}
  \item{alpha}{opacity}
  \item{col}{plot color}
  \item{apiKey}{optional API key (allows for higher rate of downloads for Google); mandatory for Bing maps}
  \item{verbose}{level of verbosity}
  \item{\dots}{further arguments to be passed to \code{PlotOnStaticMap}}
}



\author{Markus Loecher}




\examples{
if (0){
  #####################Google maps#############################
  mapBG1 = plotmap("Brandenburg Gate, Berlin", zoom = 15)

  #####################bing maps#############################
  
  #for bing maps you will need your own API key, 
  #sign up at https://msdn.microsoft.com/en-us/library/ff428642.aspx
  apiKey = scan("C:/Users/loecherm/Dropbox/stuff/bingAPIkey.txt",what="")
  mapBG2 = plotmap("Brandenburg Gate, Berlin", zoom = 15, API = "bing", apiKey=apiKey)
  
  latlon <- cbind.data.frame(lat = c(38.898648,38.889112, 38.880940), 
                              lon = c(-77.037692, -77.050273, -77.03660));
  
  
  map3 = plotmap(lat = latlon$lat, lon = latlon$lon, API = "bing", apiKey=apiKey,
                 col = "purple", pch="X",cex=1.5)
  
  
  #####################OSM maps#############################
  map4 = plotmap(lat = latlon$lat, lon = latlon$lon, API = "OSM", zoom=15,
                 col = "purple", pch="X",cex=1.5)
}
}
