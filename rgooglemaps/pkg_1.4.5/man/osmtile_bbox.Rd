\name{osmtile_bbox}
\alias{osmtile_bbox}
\title{compute the bounding box of an OpenStreetmap tile}
\description{inspired by \code{osmtile} from the package \code{OpenStreetmap}
returns the Mercator projection bounding box}
\usage{osmtile_bbox(x = 61, y = 41, zoom = 7, minim = -20037508)}
\arguments{
  \item{x}{x tile coordinate}
  \item{y}{x tile coordinate}
  \item{zoom}{zoom level}
  \item{minim}{parameter for OSM projection}
}

\value{bounding box, Mercator projection }

\author{Markus Loecher}





