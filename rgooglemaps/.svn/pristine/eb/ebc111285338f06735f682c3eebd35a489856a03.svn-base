\name{getGeoCode}
\alias{getGeoCode}
\title{geocoding utility}
\description{Geocode your data using, R, JSON and Google Maps' Geocoding APIs 
see http://allthingsr.blogspot.de/2012/01/geocode-your-data-using-r-json-and.html
and }
\usage{getGeoCode(gcStr, verbose = 0)}
\arguments{
  \item{gcStr}{adddress to geocode}
  \item{verbose}{level of verbosity}
}

\value{returns lat/lon for address}

\author{Markus Loecher}




\examples{
getGeoCode("Brooklyn")
#You can run this on the entire column of a data frame or a data table:
DF = cbind.data.frame(address=c("Berlin,Germany", "Princeton,NJ", 
          "cadillac+mountain+acadia+national+park"), lat = NA, lon = NA)
DF <- with(DF, data.frame(address, t(sapply(DF$address, getGeoCode))))

}
