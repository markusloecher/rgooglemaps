\name{degreeAxis}
\alias{degreeAxis}
\title{axis with degrees}
\description{add an axis with degree labels}
\usage{degreeAxis(side, at, labels, MyMap, ...)}
\arguments{
  \item{side}{integer; see \link{axis}}
  \item{at}{numeric; if missing, \link{axTicks} is called for nice values; see \link{axis}}
  \item{labels}{character; if omitted labels are constructed with degree symbols, ending in N/S/E/W; in case of negative degrees, sign is reversed and S or W is added; see \link{axis}}
  \item{MyMap}{optional map object to be passed}
  \item{\dots}{optional arguments to \link{axis}}
}

\value{axis is plotted on current graph}

\author{Markus Loecher}
\note{decimal degrees are used if variation is small, instead of minutes and seconds}



\examples{
xy = cbind(x = 2 * runif(100) - 1, y = 2 * runif(100) - 1)
plot(xy,xlim=c(-1,1),ylim=c(-1,1))
degreeAxis(1)
degreeAxis(2, at = c(-1,-0.5,0,0.5,1))
}
