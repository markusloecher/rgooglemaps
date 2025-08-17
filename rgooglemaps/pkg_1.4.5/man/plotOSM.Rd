\name{plotOSM}
\alias{plotOSM}
\title{plots OSM map tiles}
\description{places tiles on plot}
\usage{plotOSM(mt, upperLeft, lowerRight, lat, lon, add = FALSE, 
    removeMargin = TRUE, verbose = 0, ...)}
\arguments{
  \item{mt}{list returned by \code{GetMapTiles}}
  \item{upperLeft}{upperLeft corner in lat/lon of the plot region}
  \item{lowerRight}{lowerRight corner in lat/lon of the plot region}
  \item{lat}{latitude values to be overlaid, if any}
  \item{lon}{longitude values to be overlaid, if any}
  \item{add}{
}
  \item{removeMargin}{
}
  \item{verbose}{level of verbosity}
  \item{\dots}{further arguments to be passed to \code{rasterImage}}
}

\value{returns map object invisibly }

\author{Markus Loecher}





