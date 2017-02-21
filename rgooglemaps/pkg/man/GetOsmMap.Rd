\name{GetOsmMap}
\alias{GetOsmMap}
\title{Query the Open Street Map server for map tiles instead of Google Maps}
\description{The querying parameters for Open Street Maps are somewhat different in this version. 
Instead of a zoom, center and size, the user supplies a scale parameter and a lat/lon bounding box. 
The scale determines the image size.}
\usage{GetOsmMap(lonR = c(-74.02132, -73.98622), latR = c(40.69983, 
    40.72595), scale = 20000, destfile = "MyTile.png", format = "png", 
    RETURNIMAGE = TRUE, GRAYSCALE = FALSE, NEWMAP = TRUE, verbose = 1, 
    ...)}
\arguments{
  \item{lonR}{longitude range}
  \item{latR}{latitude range}
  \item{scale}{Open Street map scale parameter. The larger this value, the smaller the resulting map tile in memory. There is a balance to be struck between the lat/lon bounding box and the scale parameter.}
  \item{destfile}{File to load the map image from or save to, depending on \code{NEWMAP}.}
  \item{format}{(optional) defines the format of the resulting image.}
  \item{RETURNIMAGE}{return image yes/no default: TRUE}
  \item{GRAYSCALE}{Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY}}
  \item{NEWMAP}{if TRUE, query the Google server and save to \code{destfile}, if FALSE load from destfile.}
  \item{verbose}{level of verbosity,}
  \item{\dots}{extra arguments to be used in future versions}
}

\value{map structure or URL used to download the tile.}

\author{Markus Loecher}
\note{The OSM maptile server is frequently too busy to accomodate every request, so patience is warranted.}



\examples{
if (interactive()) {
 	CologneMap <- GetOsmMap(lonR= c(6.89, 7.09), latR = c(50.87, 51), scale = 150000, 
                            destfile = "Cologne.png");
	PlotOnStaticMap(CologneMap, mar=rep(4,4), NEWMAP = FALSE, TrueProj = FALSE, axes= TRUE);
		
	PrincetonMap <- GetOsmMap(lonR= c(-74.67102, -74.63943), latR = c(40.33804,40.3556), 
                             scale = 12500, destfile = "Princeton.png");
	png("PrincetonWithAxes.png", 1004, 732)
      PlotOnStaticMap(PrincetonMap, axes = TRUE, mar = rep(4,4));
    dev.off()
 }
}
