\name{GetMap.bbox}
\alias{GetMap.bbox}
\title{GetMap bbox}
\description{Wrapper function for \link{GetMap}. Query the Google server for a static map tile, defined primarily by its lat/lon range and/or center and/or zoom. 
Multiple additional arguments allow the user to customize the map tile.}
\usage{GetMap.bbox(lonR, latR, center, size = c(640, 640), destfile = "MyTile.png", 
    MINIMUMSIZE = FALSE, RETURNIMAGE = TRUE, GRAYSCALE = FALSE, 
    NEWMAP = TRUE, zoom, verbose = 0, SCALE = 1, ...)}
\arguments{
  \item{lonR}{longitude range}
  \item{latR}{latitude range}
  \item{center}{optional center}
  \item{size}{desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels}
  \item{destfile}{File to load the map image from or save to, depending on \code{NEWMAP}.}
  \item{MINIMUMSIZE}{reduce the size of the map to its minimum size that still fits the lat/lon ranges ?}
  \item{RETURNIMAGE}{return image yes/no default: TRUE}
  \item{GRAYSCALE}{Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY}}
  \item{NEWMAP}{if TRUE, query the Google server and save to \code{destfile}, if FALSE load from destfile.}
  \item{zoom}{Google maps zoom level. optional}
  \item{verbose}{level of verbosity}
  \item{SCALE}{use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map}
  \item{\dots}{extra arguments to \link{GetMap}}
}

\value{map tile}

\author{Markus Loecher}




\examples{
 mymarkers <- cbind.data.frame(lat = c(38.898648,38.889112, 38.880940), 
          lon = c(-77.037692, -77.050273, -77.03660), size =  c('tiny','tiny','tiny'), 
          col = c('blue', 'green', 'red'), char = c('','',''));

##get the bounding box:
  bb <- qbbox(lat = mymarkers[,"lat"], lon = mymarkers[,"lon"]);
  
##download the map:
  MyMap <- GetMap.bbox(bb$lonR, bb$latR, destfile = "DC.png", GRAYSCALE =TRUE,
                markers = mymarkers);
 ##The function qbbox() basically computes a bounding box for the given lat,lon 
   #points with a few additional options such as quantile boxes, additional buffers, etc.  
  bb <- qbbox(c(40.702147,40.711614,40.718217),c(-74.015794,-74.012318,-73.998284), 
            TYPE = "all", margin = list(m=rep(5,4), TYPE = c("perc", "abs")[1]));
 ##download the map:           
MyMap <- GetMap.bbox(bb$lonR, bb$latR,destfile = "MyTile3.png", maptype = "satellite") 


}
