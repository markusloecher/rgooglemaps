\name{NumTiles}
\alias{NumTiles}
\title{computes the necessary number of tiles from a bounding box and a zoom level}
\description{computes the necessary number of tiles from a bounding box and a zoom level}
\usage{NumTiles(lonR, latR, zoom = 13, CheckExistingFiles = TRUE, 
    tileExt = ".png", tileDir = "~/mapTiles/OSM/", 
    verbose = 0)}
\arguments{
  \item{lonR}{longitude range}
  \item{latR}{latitude range,}
  \item{zoom}{zoom level}
  \item{CheckExistingFiles}{logical, if TRUE check if files already exist and only download if not!}
  \item{tileExt}{image type of tile}
  \item{tileDir}{map tiles are stored in a local directory, e.g. "~/mapTiles/Google/"}
  \item{verbose}{level of verbosity}
}

\value{tuple with number of tiles for lon and lat extent}

\author{Markus Loecher}




\examples{
if (0){
  #US bounding box:
  for (zoom in 4:15) {
    cat("OSM, zoom =", zoom, "\n")
    NumTiles(lonR=c(-135,-66), latR=c(25,54) , zoom=zoom)
  }
  for (zoom in 4:15) {
    cat("Google, zoom =", zoom, "\n")
    NumTiles(lonR=c(-135,-66), latR=c(25,54) , zoom=zoom, tileDir= "~/mapTiles/Google/")
  }
}
}
