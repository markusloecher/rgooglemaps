\name{sp_bbox}

\alias{sp_bbox}

\title{computes bounding box}

\description{The function sp_bbox computes a bounding box; it was copied from the sp package bbox function }

\usage{sp_bbox(obj)}

\arguments{

  \item{obj}{object deriving from class "Spatial", or one of classes: "Line", "Lines", "Polygon" or "Polygons", or ANY, which requires obj to be an array with at least two columns}

}



\value{two-column matrix; the first column has the minimum, the second the maximum values; rows represent the spatial dimensions}


\author{Roger Bivand}


\examples{

# just 9 points on a grid:
x <- c(1,1,1,2,2,2,3,3,3)
y <- c(1,2,3,1,2,3,1,2,3)
xy <- cbind(x,y)

sp_bbox(xy)

}

