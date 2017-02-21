\name{MapBackground}
\alias{MapBackground}
\title{get static Map from the Google server}
\description{get static Map from the Google server}
\usage{MapBackground(lat, lon, destfile, NEWMAP = TRUE, myTile, zoom = NULL, 
    size = c(640, 640), GRAYSCALE = FALSE, mar = c(0, 0, 0, 0), 
    PLOT = FALSE, verbose = 1, ...)}
\arguments{
  \item{lat}{
}
  \item{lon}{
}
  \item{destfile}{File to load the map image from or save to, depending on \code{NEWMAP}.}
  \item{NEWMAP}{if TRUE, query the Google server and save to \code{destfile}, if FALSE load from destfile.}
  \item{myTile}{map tile from previous downloads}
  \item{zoom}{Google maps zoom level.}
  \item{size}{desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels}
  \item{GRAYSCALE}{Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY}}
  \item{mar}{outer margin in plot; if you want to see axes, change the default}
  \item{PLOT}{if TRUE, leave the plotting to \link{PlotOnStaticMap}, highly recommended}
  \item{verbose}{level of verbosity}
  \item{\dots}{further arguments to be passed to \link{GetMap.bbox}}
}

\value{list containing the map tile}

\author{Markus Loecher}





