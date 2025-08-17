\name{MaxZoom}
\alias{MaxZoom}
\title{computes the maximum zoom level which will contain the given lat/lon range}
\description{computes the maximum zoom level which will contain the given lat/lon range}
\usage{MaxZoom(latrange, lonrange, size = c(640, 640))}
\arguments{
  \item{latrange}{range of latitude values}
  \item{lonrange}{range of longitude values}
  \item{size}{desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels}
}

\value{zoom level}

\author{Markus Loecher}





