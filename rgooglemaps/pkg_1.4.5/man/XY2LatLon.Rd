\name{XY2LatLon}
\alias{XY2LatLon}
\title{computes the centered coordinate transformation from lat/lon to map tile coordinates }
\description{The function XY2LatLon(MyMap, X,Y,zoom) computes the coordinate transformation from map tile coordinates to lat/lon given a map object.}
\usage{XY2LatLon(MyMap, X, Y, zoom)}
\arguments{
  \item{MyMap}{map object}
  \item{X}{latitude values to transform}
  \item{Y}{longitude values to transform}
  \item{zoom}{optional zoom level. If missing, taken from \code{MyMap}}
}

\value{properly scaled and centered (with respect to the center of \code{MyMap} ) coordinates  
\item{lon }{longitude}
\item{lat }{latitude}}

\author{Markus Loecher}



\seealso{\link{LatLon2XY} \link{Tile2R}}
\examples{
#quick test:

  zoom=12;MyMap <- list(40,-120,zoom, url="google", BBOX = list(ll=c(35,-125), ur=c(45,-115)));
  LatLon <- c(lat = 40.0123, lon = -120.0123);
  Rcoords <- LatLon2XY.centered(MyMap,LatLon["lat"],LatLon["lon"])
  newLatLon <- XY2LatLon(MyMap, Rcoords$newX, Rcoords$newY)
  max(abs(newLatLon - LatLon));

#more systematic:
 for (zoom in 2:10){
   cat("zoom: ", zoom, "\n");
   MyMap <- list(40,-120,zoom, url="google", BBOX = list(ll=c(35,-125), ur=c(45,-115)));
   LatLon <- c(lat = runif(1,-80,80), lon = runif(1,-170,170));
   Rcoords <- LatLon2XY.centered(MyMap,LatLon["lat"],LatLon["lon"])
   newLatLon <- XY2LatLon(MyMap, Rcoords$newX, Rcoords$newY)
   if(max(abs(newLatLon - LatLon)) > 0.0001) print(rbind(LatLon, newLatLon));
 }

}
