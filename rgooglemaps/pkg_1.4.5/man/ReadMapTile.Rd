\name{ReadMapTile}
\alias{ReadMapTile}
\title{Read a bitmap image stored in the PNG format}
\description{Reads an image from a PNG file/content into a raster array.}
\usage{ReadMapTile(destfile, METADATA = TRUE, native = TRUE)}
\arguments{
  \item{destfile}{png file to read}
  \item{METADATA}{read MetaInfo as well ?}
  \item{native}{determines the image representation - if FALSE then the result is an array, if TRUE then the result is a native raster representation, see \link{readPNG} in package png.}
}

\value{map or tile object}

\author{Markus Loecher}





