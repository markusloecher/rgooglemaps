\name{LatLon2XY}
\alias{LatLon2XY}
\title{computes the coordinate transformation from lat/lon to map tile coordinates }
\description{The function LatLon2XY(lat,lon,zoom) computes the coordinate transformation from lat/lon to map tile coordinates given a zoom level. 
It returns the tile coordinates as well as the pixel coordinates within the Tile itself. }
\usage{LatLon2XY(lat, lon, zoom)}
\arguments{
  \item{lat}{latitude values to transform}
  \item{lon}{longitude values to transform}
  \item{zoom}{zoom level.lat,lon,zoom}
}

\value{A list with values
\item{Tile}{integer numbers specifying the tile}
\item{Coords}{pixel coordinate within the Tile}}

\author{Markus Loecher}
\note{The fractional part times 256 is the pixel coordinate within the Tile itself. }



\examples{
LatLon2XY(38.45, -122.375, 11)
}
