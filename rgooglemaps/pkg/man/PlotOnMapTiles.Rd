\name{PlotOnMapTiles}
\alias{PlotOnMapTiles}
\title{plots on map tiles by "stitching" them together}
\description{Counterpart to \code{PlotOnStaticMap} for map tiles}
\usage{PlotOnMapTiles(mt, lat, lon, center, size = c(768, 768), add = FALSE, 
    FUN = points, mar = c(0, 0, 0, 0), verbose = 0, ...)}
\arguments{
  \item{mt}{list returned by \code{GetMapTiles}}
  \item{lat}{latitude values to be overlaid, if any}
  \item{lon}{longitude values to be overlaid, if any}
  \item{center}{optional center}
  \item{size}{size (in pixels) of "stitched" map}
  \item{add}{start a new plot or add to an existing}
  \item{FUN}{plotting function to use for overlay; typical choices would be \link{points} and \link{lines}}
  \item{mar}{outer margin in plot; if you want to see axes, change the default}
  \item{verbose}{level of verbosity}
  \item{\dots}{further arguments to be passed to \code{FUN}}
}

\value{nothing returned}

\author{Markus Loecher}




\examples{

if (0){
  lat = c(40.702147,40.718217,40.711614);
  lon = c(-74.012318,-74.015794,-73.998284);
  center = c(mean(lat), mean(lon));
  zoom <- min(MaxZoom(range(lat), range(lon)));
  bb=qbbox(lat,lon)
  
  mt = GetMapTiles(latR =bb$latR , lonR=bb$lonR,zoom=zoom,verbose=1)
  PlotOnMapTiles(mt,lat=lat,lon=lon,pch=20,col=c('red', 'blue', 'green'),cex=2)
  
  mt = GetMapTiles(latR =bb$latR , lonR=bb$lonR,zoom=zoom,
                   tileDir= "~/mapTiles/Google/")
  PlotOnMapTiles(mt,lat=lat,lon=lon,pch=20,col=c('red', 'blue', 'green'),cex=2)
  
}
}
