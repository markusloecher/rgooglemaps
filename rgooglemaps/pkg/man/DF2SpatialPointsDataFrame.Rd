\name{DF2SpatialPointsDataFrame}
\alias{DF2SpatialPointsDataFrame}
\title{change data.frame to SpatialPointsDataFrame}
\description{This function modifies an object of class data.frame to one of class SpatialPointsDataFrame}
\usage{DF2SpatialPointsDataFrame(x, coords = c("x", "y"), crs = sp::CRS("+init=epsg:28992"))}
\arguments{
  \item{x}{data frame to be converted}
  \item{coords}{which columns are coordinates}
  \item{crs}{projection scheme}
}

\value{the new object of class SpatialPointsDataFrame}

\author{Markus Loecher}




\examples{
if (requireNamespace("sp", quietly = TRUE)) {
  data("meuse", package = "sp", envir = environment())
  meuseSP = DF2SpatialPointsDataFrame(meuse)
  
  sp::plot(meuseSP, asp = 1, cex = 4 * meuse$zinc/max(meuse$zinc),
       pch = 1, col = as.numeric(meuse$ffreq)+1 )
  data("meuse.riv", package = "sp", envir = environment())
  lines(meuse.riv)  
} else {
  print("package sp must be installed for this example")
}


}
