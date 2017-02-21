\name{GetMap}
\alias{GetMap}
\title{download a static map from the Google server}
\description{Query the Google server for a static map tile, defined primarily by its 
center and zoom. Many additional arguments allow the user to customize 
the map tile.
documentation at https://developers.google.com/maps/documentation/staticmaps/}
\usage{GetMap(center = c(lat = 42, lon = -76), size = c(640, 640), destfile, 
    zoom = 12, markers, path = "", span, frame, hl, sensor = "true", 
    maptype = c("roadmap", "mobile", "satellite", "terrain", 
        "hybrid", "mapmaker-roadmap", "mapmaker-hybrid")[2], 
    format = c("gif", "jpg", "jpg-baseline", "png8", "png32")[5], 
    extraURL = "", RETURNIMAGE = TRUE, GRAYSCALE = FALSE, NEWMAP = TRUE, 
    SCALE = 1, API_console_key = NULL, verbose = 0)}
\arguments{
  \item{center}{optional center (lat first,lon second  )}
  \item{size}{desired size of the map tile image. defaults to maximum size returned by the Gogle server, which is 640x640 pixels}
  \item{destfile}{File to load the map image from or save to, depending on \code{NEWMAP}.}
  \item{zoom}{Google maps zoom level.}
  \item{markers}{(optional) defines one or more markers to attach to the image at specified locations. This parameter takes a string of marker definitions separated by the pipe character (|)}
  \item{path}{(optional) defines a single path of two or more connected points to overlay on the image at specified locations. This parameter takes a string of point definitions separated by the pipe character (|)}
  \item{span}{(optional) defines a minimum viewport for the map image expressed as a latitude and longitude pair. The static map service takes this value and produces a map of the proper zoom level to include the entire provided span value from the map`s center point. Note that the resulting map may include larger bounds for either latitude or longitude depending on the rectangular dimensions of the map. If zoom is specified, span is ignored}
  \item{frame}{(optional) specifies that the resulting image should be framed with a colored blue border. The frame consists of a 5 pixel, 55 \% opacity blue border.}
  \item{hl}{(optional) defines the language to use for display of labels on map tiles. Note that this paramater is only supported for some country tiles; if the specific language requested is not supported for the tile set, then the default language for that tile set will be used.}
  \item{sensor}{specifies whether the application requesting the static map is using a sensor to determine the user`s location. This parameter is now required for all static map requests.}
  \item{maptype}{defines the type of map to construct. There are several possible maptype values, including satellite, terrain, hybrid, and mobile.}
  \item{format}{(optional) defines the format of the resulting image. By default, the Static Maps API creates GIF images. There are several possible formats including GIF, JPEG and PNG types. Which format you use depends on how you intend to present the image. JPEG typically provides greater compression, while GIF and PNG provide greater detail. This version supports only PNG.}
  \item{extraURL}{custom URL suffix}
  \item{RETURNIMAGE}{return image yes/no default: TRUE}
  \item{GRAYSCALE}{Boolean toggle; if TRUE the colored map tile is rendered into a black & white image, see \link{RGB2GRAY}}
  \item{NEWMAP}{if TRUE, query the Google server and save to \code{destfile}, if FALSE load from destfile.}
  \item{SCALE}{use the API's scale parameter to return higher-resolution map images. The scale value is multiplied with the size to determine the actual output size of the image in pixels, without changing the coverage area of the map}
  \item{API_console_key}{optional API key (allows for higher rate of downloads)}
  \item{verbose}{level of verbosity}
}

\value{map structure or URL used to download the tile.}

\author{Markus Loecher}
\note{Note that size is in order (lon, lat)}


\seealso{\link{GetMap.bbox}}
\examples{
  lat = c(40.702147,40.718217,40.711614);
  lon = c(-74.012318,-74.015794,-73.998284);
  center = c(mean(lat), mean(lon));
  zoom <- min(MaxZoom(range(lat), range(lon)));
  #this overhead is taken care of implicitly by GetMap.bbox(); 
  markers = paste0("&markers=color:blue|label:S|40.702147,-74.015794&markers=color:",
                   "green|label:G|40.711614,-74.012318&markers=color:red|color:red|",
                   "label:C|40.718217,-73.998284")
  MyMap <- GetMap(center=center, zoom=zoom,markers=markers);
  #Note that in the presence of markers one often needs to add some extra padding to the 
  #latitude range to accomodate the extent of the top most marker
  if (0){#takes too long to run for CRAN check
  #add a path, i.e. polyline:
MyMap <- GetMap(center=center, zoom=zoom,
  path = paste0("&path=color:0x0000ff|weight:5|40.737102,-73.990318|",
  "40.749825,-73.987963|40.752946,-73.987384|40.755823,-73.986397"));
  #use implicit geo coding 
  BrooklynMap <- GetMap(center="Brooklyn", zoom=13)
  PlotOnStaticMap(BrooklynMap)
  
  #use implicit geo coding and display labels in Korean:
  BrooklynMap <- GetMap(center="Brooklyn", zoom=13, hl="ko")
  PlotOnStaticMap(BrooklynMap)
  
  #no highways
  ManHatMap <- GetMap(center="Lower Manhattan", zoom=14, 
                      extraURL="&style=feature:road.highway|visibility:off")
  PlotOnStaticMap(ManHatMap)
  
   #The example below defines a polygonal area within Manhattan, passed a series of 
  #intersections as locations:
#MyMap <- GetMap(path = paste0("&path=color:0x00000000|weight:5|fillcolor:0xFFFF0033|",
#          "8th+Avenue+\%26+34th+St,New+York,NY|8th+Avenue+\%26+42nd+St,New+York,NY|",
#          "Park+Ave+\%26+42nd+St,New+York,NY,NY|Park+Ave+\%26+34th+St,New+York,NY,NY"),
#            destfile = "MyTile3a.png");

  #note that since the path string is just appended to the URL you can "abuse" the path 
  #argument to pass anything to the query, e.g. the style parameter:
  #The following example displays a map of Brooklyn where local roads have been changed 
  #to bright green and the residential areas have been changed to black:
  # MyMap <- GetMap(center="Brooklyn", zoom=12, maptype = "roadmap", 
  #path = paste0("&style=feature:road.local|element:geometry|hue:0x00ff00|",
  #        "saturation:100&style=feature:landscape|element:geometry|lightness:-100"),
  #        sensor='false', destfile = "MyTile4.png",  RETURNIMAGE = FALSE);
   
   #In the last example we set RETURNIMAGE to FALSE which is a useful feature in general
  #if png is not installed. In that cases, the images can still be fetched 
  #and saved but not read into R.

  #In the following example we let the Static Maps API determine the correct center and 
  #zoom level implicitly, based on evaluation of the position of the markers. 
  #However, to be of use within R we do need to know the values for zoom and 
  #center explicitly, so it is better practice to compute them ourselves and 
  #pass them as arguments, in which case meta information on the map tile can be saved as well.
  
  #MyMap <- GetMap(markers = paste0("&markers=color:blue|label:S|40.702147,-74.015794&",
  #          "markers=color:green|label:G|40.711614,-74.012318&markers=color:red|",
  #          "color:red|label:C|40.718217,-73.998284"), 
  #           destfile = "MyTile1.png",  RETURNIMAGE = FALSE);
  }
}
