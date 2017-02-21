\name{RGB2GRAY}
\alias{RGB2GRAY}
\title{translates an RGB image matrix to gray scale}
\description{This function translates the rgb values of the array myTile into a scalar matrix with just one gray value per pixel.}
\usage{RGB2GRAY(myTile)}
\arguments{
  \item{myTile}{rgb image matrix, usually array with 3 dimensions}
}
\details{Gray scale intensity defined as  0.30R + 0.59G + 0.11B}
\value{image tile}

\author{Markus Loecher}




\examples{
if (interactive()){
  BrooklynLatLon = getGeoCode("Brooklyn")
  mapBrooklyn <- GetMap(center=BrooklynLatLon, destfile = file.path(tempdir(), "Brooklyn.png"), 
                 zoom=11, size = c(240,240))
  mapBrooklynBW$myTile = RGB2GRAY(mapBrooklyn$myTile)
  PlotOnStaticMap(mapBrooklynBW)
}
}
