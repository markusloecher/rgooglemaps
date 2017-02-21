\name{NYleukemia}
\alias{NYleukemia}
\docType{data}
\title{Upstate New York Leukemia Data}
\description{
Census tract level (\code{n=281}) leukemia data for the 8 counties in upstate New York from 1978-1982, paired with population data from the 1980 census.
}
\usage{data(NYleukemia)}
\format{
List with 5 items:
\tabular{ll}{
\code{geo} \tab table of the FIPS code, longitude, and latitude of the geographic centroid of each census tract \cr
\code{data} \tab table of the FIPS code, number of cases, and population of each census tract \cr
\code{spatial.polygon} \tab object of class SpatialPolygons (See \link[sp]{SpatialPolygons-class}) containing a map of the study region \cr
\code{surrounded} \tab row IDs of the 4 census tracts that are completely surrounded by the \code{surrounding} census tracts \cr
\code{surrounding} \tab row IDs of the 4 census tracts that completely surround the \code{surrounded} census tracts \cr
}
}

\source{http://www.sph.emory.edu/~lwaller/ch4index.htm}
\references{
Turnbull, B. W. et al (1990)
Monitoring for clusters of disease: application to leukemia incidence in upstate New York \emph{American Journal of Epidemiology}, \bold{132}, 136--143
}
\examples{
data(NYleukemia)
  population <- NYleukemia$data$population
  cases <- NYleukemia$data$cases
  mapNY <- GetMap(center=c(lon=-76.00365, lat=42.67456), destfile = "NYstate.png",
  maptype = "mobile", zoom=9)
  ColorMap(100*cases/population, mapNY, NYleukemia$spatial.polygon, add = FALSE,
  alpha = 0.35, log = TRUE, location = "topleft")
 
}
\keyword{datasets}
