\name{getGeoCode}
\alias{getGeoCode}
\title{geocoding utility}
\description{Geocode your data using, R, JSON and OSM or Google Maps' Geocoding APIs }
\usage{getGeoCode(gcStr, API = c("osm", "google")[1], JSON = FALSE, 
    verbose = 0)}
\arguments{
  \item{gcStr}{adddress to geocode}
  \item{API}{which API to use. see https://nominatim.org/release-docs/develop/api/Search/ and http://allthingsr.blogspot.de/2012/01/geocode-your-data-using-r-json-and.html}
  \item{JSON}{use the JSON protocol. If FALSE, we do not have to load additional libraries}
  \item{verbose}{level of verbosity}
}

\value{returns lat/lon for address}

\author{Markus Loecher}




\examples{
if (0){
  getGeoCode("1600 Amphitheatre Parkway, Mountain View, CA")
  getGeoCode("Brooklyn")
  #You can run this on the entire column of a data frame or a data table:
  DF = cbind.data.frame(address=c("Berlin,Germany", "Princeton,NJ", 
            "cadillac+mountain+acadia+national+park"), lat = NA, lon = NA)
  DF <- with(DF, data.frame(address, t(sapply(DF$address, getGeoCode))))
}
}
