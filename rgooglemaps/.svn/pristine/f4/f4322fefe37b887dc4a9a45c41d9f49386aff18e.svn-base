\name{GetBingMap}
\alias{GetBingMap}
\title{download a static map from the Microsoft map tile server}
\description{Query the Google server for a static map tile, defined primarily by its 
center and zoom. Many additional arguments allow the user to customize 
the map tile.}
\usage{GetBingMap(center = c(lat = 42, lon = -76), mapArea = c(45.219, 
    -122.325, 47.61, -122.107), size = c(640, 640), destfile, 
    zoom = 12, markers, path = "", maptype = c("Road", "Aerial ", 
        "AerialWithLabels")[1], format = c("png", "gif", "jpg", 
        "jpg-baseline", "png8", "png32")[1], RETURNIMAGE = TRUE, 
    GRAYSCALE = FALSE, NEWMAP = TRUE, SCALE = 1, apiKey = NULL, 
    verbose = 0)}
\arguments{
  \item{center}{optional center (lat first,lon second  )}
  \item{mapArea}{A rectangular area specified as a bounding box (ll,ur). Required when a center point or set of route points are not specified}
  \item{size}{desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels}
  \item{destfile}{File to load the map image from or save to, depending on \code{NEWMAP}.}
  \item{zoom}{Google maps zoom level.}
  \item{markers}{(optional) defines one or more markers to attach to the image at specified locations. This parameter takes a string of marker definitions separated by the pipe character (|)}
  \item{path}{(optional) defines a single path of two or more connected points to overlay on the image at specified locations. This parameter takes a string of point definitions separated by the pipe character (|)}
  \item{maptype}{defines the type of map to construct. See https://msdn.microsoft.com/en-us/library/ff701724.aspx}
  \item{format}{(optional) defines the format of the resulting image. By default, the Static Maps API creates GIF images. There are several possible formats including GIF, JPEG and PNG types. Which format you use depends on how you intend to present the image. JPEG typically provides greater compression, while GIF and PNG provide greater detail. This version supports only PNG.}
  \item{RETURNIMAGE}{return image yes/no default: TRUE}
  \item{GRAYSCALE}{Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY}}
  \item{NEWMAP}{if TRUE, query the Google server and save to \code{destfile}, if FALSE load from destfile.}
  \item{SCALE}{use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map}
  \item{apiKey}{optional API key (allows for higher rate of downloads)}
  \item{verbose}{level of verbosity}
}

\value{map structure or URL used to download the tile.}

\author{Markus Loecher}
\note{Note that size is in order (lon, lat)}


\seealso{\link{GetMap.bbox}}
\examples{

if (0){
  #for bing maps you will need your own API key, sign up at https://msdn.microsoft.com/en-us/library/ff428642.aspx
  apiKey = scan("C:/Users/loecherm/Dropbox/stuff/bingAPIkey.txt")
  map1=GetBingMap(center=c(47.619048,-122.35384),zoom=15,apiKey=apiKey, verbose=1, destfile="Seattle.png") 
  PlotOnStaticMap(map1)
  m="&pp=47.620495,-122.34931;21;AA&pp=47.619385,-122.351485;;AB&pp=47.616295,-122.3556;22"
  map2=GetBingMap(center=c(47.619048,-122.35384),zoom=15,markers=m,apiKey=apiKey, verbose=1, destfile="Seattle2.png")
  
  PlotOnStaticMap(map2,lat=c(47.620495,47.619385,47.616295),lon=c(-122.34931,-122.351485,-122.3556))
  	
  m="&pp=49.28273,-123.12074;22&pp=44.05207,-123.08675;22"
  #http://dev.virtualearth.net/REST/v1/Imagery/Map/Road/47.677006,-122.125526/admindivision1?ms=500,270&key=
  map3= GetBingMap(center=c(47.677006,-122.125526),zoom=6,markers=m,apiKey=apiKey, verbose=1, destfile="Seattle2.png")
  #plotmap(map=map3)
  m=cbind.data.frame(lat=c(49.28273,44.05207),lon=c(-123.12074,-123.08675),col=c(3:4))
  PlotOnStaticMap(map3, lat =m$lat,lon=m$lon,col=m$col,pch=19)
}
}
