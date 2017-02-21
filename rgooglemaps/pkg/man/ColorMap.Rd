\name{ColorMap}
\alias{ColorMap}
\title{Plot Levels of a Variable in a Colour-Coded Map}
\description{Plot Levels of a Variable in a Colour-Coded Map}
\usage{ColorMap(values, map = NULL, polys = NULL, log = FALSE, nclr = 7, 
    include.legend = list(TRUE), round = 3, brks = NULL, legend = NULL, 
    location = "topright", rev = FALSE, alpha = 0.5, GRAY = FALSE, 
    palette = c("YlOrRd", "RdYlGn", "Spectral")[1], textInPolys = NULL, 
    ...)}
\arguments{
  \item{values}{variable to plot}
  \item{map}{map object}
  \item{polys}{an object of class SpatialPolygons (See \link[sp]{SpatialPolygons-class}}
  \item{log}{boolean of whether to plot values on log scale}
  \item{nclr}{number of colour-levels to use}
  \item{include.legend}{boolean of whether to include legend}
  \item{round}{number of digits to round to in legend}
  \item{brks}{if desired, pre-specified breaks for legend}
  \item{legend}{if desired, a pre-specified legend}
  \item{location}{location of legend}
  \item{rev}{boolean of whether to reverse colour scheme (darker colours for smaller values)}
  \item{alpha}{alpha value of colors}
  \item{GRAY}{boolean: if TRUE, use gray scale instead}
  \item{palette}{palette to choose from RColorBrewer}
  \item{textInPolys}{text to be displayed inside polygons. This can be a column names for values}
  \item{\dots}{extra args to pass to \code{PlotPolysOnStaticMap}}
}



\author{Markus Loecher}




\examples{
  if (interactive()){
  data("NYleukemia", envir = environment())
  population <- NYleukemia$data$population
  cases <- NYleukemia$data$cases
  mapNY <- GetMap(center=c(lat=42.67456,lon=-76.00365), destfile = "NYstate.png", 
                  maptype = "mobile", zoom=9)
  ColorMap(100*cases/population, mapNY, NYleukemia$spatial.polygon, add = FALSE,
           alpha = 0.35, log = TRUE, location = "topleft")
}
  #ColorMap(100*cases/population, map=NULL, NYleukemia$spatial.polygon)
  
}
