\name{qbbox}
\alias{qbbox}
\title{computes bounding box}
\description{The function qbbox computes a bounding box for the given lat,lon 
points with a few additional options such as quantile boxes, additional margins, etc.}
\usage{qbbox(lat, lon, TYPE = c("all", "quantile")[1], margin = list(m = c(1, 
    1, 1, 1), TYPE = c("perc", "abs")[1]), q.lat = c(0.1, 0.9), 
    q.lon = c(0.1, 0.9), verbose = 0)}
\arguments{
  \item{lat}{latitude values}
  \item{lon}{longitude values}
  \item{TYPE}{
}
  \item{margin}{
}
  \item{q.lat}{
}
  \item{q.lon}{
}
  \item{verbose}{
}
}

\value{\item{latR }{latitude range}
\item{lonR }{longitude range}}

\author{Markus Loecher}




\examples{
lat = 37.85 + rnorm(100, sd=0.001);
lon = -120.47 + rnorm(100, sd=0.001);
#add a few outliers:
lat[1:5] <- lat[1:5] + rnorm(5, sd =.01);
lon[1:5] <- lon[1:5] + rnorm(5, sd =.01);

#range, discarding the upper and lower 10\% of the data
qbbox(lat, lon, TYPE = "quantile");
#full range:
qbbox(lat, lon, TYPE = "all");
#add a 10\% extra margin on all four sides:
qbbox(lat, lon, margin = list(m = c(10, 10, 10, 10), TYPE = c("perc", "abs")[1]));
 
}
