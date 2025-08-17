\name{Tile2R}
\alias{Tile2R}
\title{simple utility to offset and scale XY coordinates with respect to the center}
\description{simple utility to offset and scale XY coordinates with respect to the center}
\usage{Tile2R(points, center)}
\arguments{
  \item{points}{XY coordinates returned by e.g. \link{LatLon2XY}}
  \item{center}{XY coordinates of center returned by e.g. \link{LatLon2XY}}
}
\details{mainly used for shrinking the size of a tile to the minimum size.}
\value{list with X and Y pixel values}

\author{Markus Loecher}




\examples{
latR <- c(34.5,34.9);
 lonR <- c(-100.3, -100);
 lat.center <- 34.7;
 lon.center <- -100.2;
 zoom = 10;
 ll <- LatLon2XY(latR[1], lonR[1], zoom);#lower left corner
 ur <- LatLon2XY(latR[2], lonR[2], zoom );#upper right corner
 cr <- LatLon2XY(lat.center, lon.center, zoom );#center
 ll.Rcoords <- Tile2R(ll, cr);
 ur.Rcoords <- Tile2R(ur, cr);

}
