\name{SpatialToPBS}
\alias{SpatialToPBS}
\title{converts spatial objects as defined in package sp to simpler PBSmapping type dataframes }
\description{The PlotPolysOnStaticMap() function currently does not take sp objects directly but instead needs
PBSmapping type data.frames. This function converts sp objects into such.                         
THANKS TO Fabio Priuli for a major bug fix w.r.t. holes in spatial polygons!}
\usage{SpatialToPBS(xy, verbose = 0)}
\arguments{
  \item{xy}{spatial object, such as SpatialPoints, SpatialPolygons, etc..}
  \item{verbose}{level of verbosity}
}

\value{list with elements xy = converted object, bb = bounding box, fun = plot function}

\author{Markus Loecher}




\examples{
  if (interactive()) {
  data("NYleukemia", envir = environment())
  population <- NYleukemia$data$population
  cases <- NYleukemia$data$cases
  mapNY <- GetMap(center=c(lat=42.67456,lon=-76.00365), 
                  destfile = file.path(tempdir(),"NYstate.png"), 
                  maptype = "mobile", zoom=9)
  #mapNY=ReadMapTile("NYstate.png")
  clrStuff=ColorMap(100*cases/population, alpha = 0.35, log = TRUE)
  NYpolys = SpatialToPBS(NYleukemia$spatial.polygon)
  PlotPolysOnStaticMap(mapNY, NYpolys$xy, col = clrStuff$colcode, add = FALSE)
  legend("topleft", legend = clrStuff$legend, fill = clrStuff$fill, 
         bg = rgb(0.1,0.1,0.1,0.3))
}
  
}
