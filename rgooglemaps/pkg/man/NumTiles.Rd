\name{NumTiles}
\alias{NumTiles}
\title{computes the necessary number of tiles from a bounding box and a zoom level}
\description{computes the necessary number of tiles from a bounding box and a zoom level}
\usage{NumTiles(lonR, latR, zoom = 13, CheckExistingFiles = TRUE, tileExt = ".png", 
    tileDir = "~/mapTiles/OSM/", verbose = 0)}
\arguments{
  \item{lonR}{longitude range}
  \item{latR}{latitude range,}
  \item{zoom}{zoom level}
  \item{CheckExistingFiles}{logical, if TRUE check if files already exist and only download if not!}
  \item{tileExt}{image type of tile}
  \item{tileDir}{map tiles are stored in a local directory}
  \item{verbose}{level of verbosity}
}

\value{tuple with number of tiles for lon and lat extent}

\author{Markus Loecher}




\examples{
#US bounding box:
NumTiles(lonR=c(-135,-66), latR=c(25,54) , zoom=8)

}
