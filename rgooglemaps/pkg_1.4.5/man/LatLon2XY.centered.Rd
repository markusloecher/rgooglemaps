\name{LatLon2XY.centered}
\alias{LatLon2XY.centered}
\title{computes the centered coordinate transformation from lat/lon to map tile coordinates }
\description{The function LatLon2XY.centered(MyMap, lat,lon,zoom) computes the coordinate transformation from lat/lon to map tile coordinates given a map object.}
\usage{LatLon2XY.centered(MyMap, lat, lon, zoom)}
\arguments{
  \item{MyMap}{map object}
  \item{lat}{latitude values to transform}
  \item{lon}{longitude values to transform}
  \item{zoom}{optional zoom level. If missing, taken from \code{MyMap}}
}

\value{properly scaled and centered (with respect to the center of \code{MyMap} ) coordinates  
\item{newX }{ transformed longitude}
\item{newY }{transformed latitude}}

\author{Markus Loecher}



\seealso{\link{LatLon2XY} \link{Tile2R} }

