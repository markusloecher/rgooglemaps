\name{NumTiles}
\alias{NumTiles}
\title{computes the necessary number of tiles from a bounding box and a zoom level}
\description{computes the necessary number of tiles from a bounding box and a zoom level}
\usage{NumTiles(lonR, latR, zoom = 13)}
\arguments{
  \item{lonR}{longitude range}
  \item{latR}{latitude range,}
  \item{zoom}{zoom level}
}

\value{tuple with number of tiles for lon and lat extent}

\author{Markus Loecher}




\examples{
#US bounding box:
NumTiles(lonR=c(-135,-66), latR=c(25,54) , zoom=8)

}
