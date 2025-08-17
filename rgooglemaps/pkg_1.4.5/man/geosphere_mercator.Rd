\name{geosphere_mercator}
\alias{geosphere_mercator}
\title{Transform longitude/latiude points to the Mercator projection. }
\description{From \code{geosphere::mercator}}
\usage{geosphere_mercator(p, inverse = FALSE, r = 6378137)}
\arguments{
  \item{p}{longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude)}
  \item{inverse}{Logical. If TRUE, do the inverse projection (from Mercator to longitude/latitude}
  \item{r}{Numeric. Radius of the earth; default = 6378137 m}
}

\value{Mercator projection of lon/lat points}

\author{Markus Loecher}





