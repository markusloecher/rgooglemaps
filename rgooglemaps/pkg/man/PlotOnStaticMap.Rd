\name{PlotOnStaticMap}
\alias{PlotOnStaticMap}
\title{overlays plot on background image of map tile}
\description{This function is the workhorse of the package RgoogleMaps. It overlays plot on background image of map tile}
\usage{PlotOnStaticMap(MyMap, lat, lon, destfile, zoom = NULL, size, 
    GRAYSCALE = FALSE, add = FALSE, FUN = points, mar = c(0, 
        0, 0, 0), NEWMAP = TRUE, TrueProj = TRUE, axes = FALSE, 
    atX = NULL, atY = NULL, verbose = 0, ...)}
\arguments{
  \item{MyMap}{optional map object}
  \item{lat}{latitude values to be overlaid}
  \item{lon}{longitude values to be overlaid}
  \item{destfile}{File to load the map image from or save to, depending on whether \code{MyMap} was passed.}
  \item{zoom}{Google maps zoom level. optional if \code{MyMap} is passed, required if not.}
  \item{size}{desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels}
  \item{GRAYSCALE}{Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY}}
  \item{add}{start a new plot or add to an existing}
  \item{FUN}{plotting function to use for overlay; typical choices would be \link{points} and \link{lines}}
  \item{mar}{outer margin in plot; if you want to see axes, change the default}
  \item{NEWMAP}{load map from file or get it "new" from the static map server}
  \item{TrueProj}{set to FALSE if you are willing to accept some degree of inaccuracy in the mapping. In that case, the coordinates of the image are in lat/lon and the user can simply overly points/lines/axis without worrying about projections}
  \item{axes}{overlay axes ?}
  \item{atX}{numeric; position of ticks on x-axis; if missing, \link{axTicks} is called for nice values; see \link{axis}}
  \item{atY}{numeric; position of ticks on y-axis; if missing, \link{axTicks} is called for nice values; see \link{axis}}
  \item{verbose}{level of verbosity}
  \item{\dots}{further arguments to be passed to \code{FUN}}
}

\value{the map object is returned via \code{invisible(MyMap)}}

\author{Markus Loecher}




\examples{
#The first step naturally will be to download a static map from the Google server. A simple example:

  lat = c(40.702147,40.711614,40.718217);
  lon = c(-74.015794,-74.012318,-73.998284);
  center = c(mean(lat), mean(lon));
  zoom <- min(MaxZoom(range(lat), range(lon)));
  #this overhead is taken care of implicitly by GetMap.bbox();              
  MyMap <- GetMap(center=center, zoom=zoom,markers = paste0("&markers=color:blue|label:S|",
           "40.702147,-74.015794&markers=color:green|label:G|40.711614,-74.012318&markers=",
           "color:red|color:red|label:C|40.718217,-73.998284"), destfile = "MyTile1.png");
                 
   tmp <- PlotOnStaticMap(MyMap, lat = lat, 
                          lon = lon, 
                          destfile = "MyTile1.png", cex=1.5,pch=20,
                          col=c('red', 'blue', 'green'), add=FALSE);
   #and add lines:
   PlotOnStaticMap(MyMap, lat = c(40.702147,40.711614,40.718217), 
                   lon = c(-74.015794,-74.012318,-73.998284), 
                   lwd=1.5,col=c('red', 'blue', 'green'), FUN = lines, add=TRUE)
   	

}
