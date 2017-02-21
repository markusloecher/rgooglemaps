\name{bubbleMap}
\alias{bubbleMap}
\title{Create a bubble plot of spatial data on Google Maps}
\description{This function creates a bubble plot of spatial
data, with options for bicolour residual plots.}
\usage{bubbleMap(SP, coords = c("x", "y"), crs = sp::CRS("+proj=longlat +datum=WGS84"), 
    map, filename = "", zcol = 1, max.radius = 100, key.entries, 
    do.sqrt = TRUE, colPalette = NULL, strokeColor = "#FFAA00", 
    alpha = 0.7, strokeWeight = 1, LEGEND = TRUE, legendLoc = "topleft", 
    verbose = 0)}
\arguments{
  \item{SP}{object of class data.frame or \link[sp]{SpatialPointsDataFrame-class} with associated coordinate reference systems}
  \item{coords}{names of coordinate columns}
  \item{crs}{coordinate reference systems}
  \item{map}{map object; if missing map is downloaded from server}
  \item{filename}{filename to save the map under, IF map object not given}
  \item{zcol}{variable column name, or column number after removing spatial coordinates from x@data: 1 refers to the first non-coordinate column}
  \item{max.radius}{value for largest circle (the plotting symbols) in metre, circumcircle of triange or quadrangle (square)}
  \item{key.entries}{value for largest circle (the plotting symbols) in metre, circumcircle of triange or quadrangle (square)}
  \item{do.sqrt}{logical; if TRUE the plotting symbol area (sqrt(diameter)) is proportional to the value of the z-variable; if FALSE, the symbol size (diameter) is proportional to the z-variable}
  \item{colPalette}{colours to be used to fill plotting symbols; numeric vector of same size like key.entries

colours to be used to fill features depending on attribute}
  \item{strokeColor}{the color to draw the border of circle (the plotting symbols)}
  \item{alpha}{the fill opacity between 0.0 and 1.0}
  \item{strokeWeight}{the stroke width in pixels}
  \item{LEGEND}{logical; if TRUE add bubbleLegend}
  \item{legendLoc}{the x and y co-ordinates to be used to position the legend. They can be specified by keyword or in any way which is accepted by \code{legend}}
  \item{verbose}{level of verbosity}
}

\value{##################################################################
map structure or URL used to download the tile.}

\author{Markus Loecher}




\examples{
data(lat.lon.meuse, package="loa", envir = environment())

map <- GetMap(center=c(lat=50.97494,lon=5.743606), zoom=13,
       size=c(480,480),destfile = file.path(tempdir(),"meuse.png"),
        maptype="mobile", SCALE = 1);

par(cex=1.5)
bubbleMap(lat.lon.meuse, coords = c("longitude","latitude"), map=map,
      zcol='zinc', key.entries = 100+ 100 * 2^(0:4));

}
