\name{IdentifyPoints}
\alias{IdentifyPoints}
\title{identify points by clicking on map}
\description{The user can try to identify lat/lon pairs on the map by clicking on them}
\usage{IdentifyPoints(MyMap, n = 1, verbose = 0)}
\arguments{
  \item{MyMap}{map object}
  \item{n}{the maximum number of points to locate.}
  \item{verbose}{level of verbosity}
}

\value{the lat/lon coordinates of the chosen points are returned }

\author{Markus Loecher}




\examples{
#The first step naturally will be to download a static map from the Google server. A simple example:

 #identifiy points:
 #IdentifyPoints(MyMap,5)

}
