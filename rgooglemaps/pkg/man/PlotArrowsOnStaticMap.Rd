\name{PlotArrowsOnStaticMap}
\alias{PlotArrowsOnStaticMap}
\title{plots arrows or segments on map }
\description{This function plots/overlays arrows or segments on a map.}
\usage{PlotArrowsOnStaticMap(MyMap, lat0, lon0, lat1 = lat0, lon1 = lon0, 
    TrueProj = TRUE, FUN = arrows, add = FALSE, verbose = 0, 
    ...)}
\arguments{
  \item{MyMap}{map image returned from e.g. \code{GetMap()}}
  \item{lat0}{latitude valuesof points FROM which to draw.}
  \item{lon0}{longitude values of points FROM which to draw.}
  \item{lat1}{latitude valuesof points TO which to draw.}
  \item{lon1}{longitude values of points TO which to draw.}
  \item{TrueProj}{set to FALSE if you are willing to accept some degree of inaccuracy in the mapping. In that case, the coordinates of the image are in lat/lon and the user can simply overly points/lines/axis without worrying about projections}
  \item{FUN}{, plotting function to use for overlay; typical choices would be \link{arrows} and \link{segments}}
  \item{add}{start a new plot or add to an existing}
  \item{verbose}{level of verbosity}
  \item{\dots}{further arguments to be passed to \code{FUN}}
}

\value{return value of \code{FUN}}

\author{Markus Loecher}



\seealso{\link{PlotOnStaticMap} \link{arrows}}
\examples{
    MyMap <- GetMap(center=c(lat=40.7,lon=-74), zoom=11)
PlotArrowsOnStaticMap(MyMap, lat0=40.69, lon0=-73.9, lat1=40.71, lon1=-74.1, col = 'red')   

}
