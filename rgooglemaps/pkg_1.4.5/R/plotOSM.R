
osmtile_bbox = function# compute the bounding box of an OpenStreetmap tile
### inspired by \code{osmtile} from the package \code{OpenStreetmap}
### returns the Mercator projection bounding box
(
  x=61, ##<< x tile coordinate
  y=41, ##<< x tile coordinate
  zoom =7, ##<< zoom level
  minim = -20037508 ##<< parameter for OSM projection
){
  sc <- abs(minim) * 2
  
  p1 <- c(x/(2^zoom) * sc + minim, -(y/(2^zoom) * sc + minim))#upper left ??
  p2 <- c((x + 1)/(2^zoom) * sc + minim, -((y + 1)/(2^zoom) * 
                                             sc + minim)) #lowerRight?
  bbox <- list(upperLeft = p1, lowerRight = p2)
  return(bbox)
### bounding box, Mercator projection 
}


plotOSM = function#plots OSM map tiles
### places tiles on plot
(
  mt, ##<< list returned by \code{GetMapTiles}
  upperLeft, ##<< upperLeft corner in lat/lon of the plot region
  lowerRight, ##<< lowerRight corner in lat/lon of the plot region
  lat, ##<< latitude values to be overlaid, if any
  lon, ##<< longitude values to be overlaid, if any
  add = FALSE, 
  removeMargin = TRUE, 
  verbose=0, ##<< level of verbosity
  ... ##<< further arguments to be passed to \code{rasterImage}
) {
  map <- list(tiles = mt)
  map$bboxLL = list()
  
  if (missing(upperLeft) & missing(lowerRight)) {
    x = range(mt$X)
    y = range(mt$Y)
    
    upperLeft = osmtile_bbox(x=x[1], y=y[1], zoom = mt$zoom)$upperLeft
    lowerRight = osmtile_bbox(x=x[2], y=y[2], zoom = mt$zoom)$lowerRight
    map$bbox <- list(upperLeft =upperLeft,lowerRight=lowerRight)
  } else {
    map$bbox <- list(upperLeft = geosphere_mercator(upperLeft[1L], upperLeft[2L]), 
                     lowerRight = geosphere_mercator(lowerRight[1L], lowerRight[2L]))
    if (verbose) print(map$bbox)
    map$bbox <- list(p1 = c(x = min(map$bbox$upperLeft[1L], map$bbox$lowerRight[1L]), 
                            y = max(map$bbox$upperLeft[2L], map$bbox$lowerRight[2L])), 
                     p2 = c(x = max(map$bbox$upperLeft[1L], map$bbox$lowerRight[1L]), 
                            y = min(map$bbox$p1[2L], map$bbox$lowerRight[2L])))
  }
  map$bboxLL$upperLeft =geosphere_mercator(map$bbox$upperLeft,inverse = TRUE)
  map$bboxLL$lowerRight =geosphere_mercator(map$bbox$lowerRight,inverse = TRUE)
  
  if (verbose) print(map$bbox)
  
    
    mar <- par("mar")
    if (add == FALSE) {
        plot.new()
        if (removeMargin) 
            par(mar = c(0, 0, 0, 0))
        xlim_p = c(map$bbox$upperLeft[1], map$bbox$lowerRight[1])
        ylim_p = c(map$bbox$lowerRight[2], map$bbox$upperLeft[2])
        plot.window(xlim_p, ylim_p, xaxs = "i", yaxs = "i", asp = T)
        if (verbose>1) browser()
    }
    #if (verbose>1) browser()
    k=1
    for (x in mt$X){
      for (y in mt$Y){
        osmtile=list()
        osmtile$tile = mt$tiles[[k]]
        osmtile$x=x;osmtile$y=y;osmtile$zoom=mt$zoom
        plotOSMtile(osmtile, verbose=verbose, ...)
        k=k+1
      }
    }

    par(mar = mar)
    invisible(map)
### returns map object invisibly 
}

plotOSMtile = function# plots a single OSM tile
### Adds tile to plot
(
  osmtile, ##<< tile object
  zoom, ##<<  zoom level
  add = TRUE, ##<< 
  raster = TRUE, ##<< 
  verbose=0, ##<< level of verbosity
  ... ##<< further arguments to be passed to \code{rasterImage}
){ 
  
  xres <- nrow(osmtile$tile) #look up: 256 or 255 ?!!
  yres <- ncol(osmtile$tile)
  if (missing(zoom)) zoom = osmtile$zoom
  
  if (verbose>1) browser()
  
  bbox=osmtile_bbox(x=osmtile$x, y=osmtile$y, zoom = zoom)
  bbox$p1 = bbox$upperLeft
  bbox$p2 = bbox$lowerRight
  m=0#0.5 #margin
  xleft = bbox$p1[1] - m * abs(bbox$p1[1] - bbox$p2[1])/yres
  ybottom = bbox$p2[2] + m * abs(bbox$p1[2] - bbox$p2[2])/xres
  xright = bbox$p2[1] - m * abs(bbox$p1[1] - bbox$p2[1])/yres
  ytop = bbox$p1[2] + m * abs(bbox$p1[2] - bbox$p2[2])/xres
  
  
  if (!raster) 
    image(x = seq(xleft, xright, length = yres), y = seq(ybottom, ytop, length = xres), 
          z = t(matrix(1:(xres * yres), nrow = xres, byrow = TRUE))[, 
                                                                    xres:1], col = as.vector(osmtile$tile), add = add, ...)
  else rasterImage(osmtile$tile, xleft, ybottom, xright, ytop, ...)
  
  if (verbose) {
    cat("placing tile at coords:",xleft, ybottom, xright, ytop, "\n")
    rect(xleft, ybottom, xright, ytop)
  }
### returns nothing
}

geosphere_mercator = function#Transform longitude/latiude points to the Mercator projection. 
### From \code{geosphere::mercator}
(
    p, ##<< longitude/latitude of point(s). Can be a vector of two numbers, a matrix of 2 columns (first one is longitude, second is latitude) 
    inverse = FALSE, ##<<  Logical. If TRUE, do the inverse projection (from Mercator to longitude/latitude
    r = 6378137  ##<< Numeric. Radius of the earth; default = 6378137 m
) {
  toRad <- pi/180
  if (inverse) {
    p <- .pToMatrix(p)
    p[, 2] <- pi/2 - 2 * atan(exp(-p[, 2]/r))
    p[, 1] <- p[, 1]/r
    colnames(p) <- c("lon", "lat")
    return(p/toRad)
  }
  else {
    p <- .pToMatrix(p) * toRad
    p[, 2] <- log(tan(p[, 2]) + (1/cos(p[, 2])))
    p <- p * r
    colnames(p) <- c("x", "y")
    return(p)
  }
### Mercator projection of lon/lat points
}

.pToMatrix <- function(p) {
  
  if (is.data.frame(p)) {
    p <- as.matrix(p)
  } else 
    
    if (is.vector(p)){
      if (length(p) != 2) {
        stop('Wrong length for a vector, should be 2')
      } else {
        p <- matrix(p, ncol=2) 
      }
    } else if (is.matrix(p)) {
      if (ncol(p) != 2) {
        stop( 'A points matrix should have 2 columns')
      }
      cn <- colnames(p)
      if (length(cn) == 2) {
        if (toupper(cn[1]) == 'Y' | toupper(cn[2]) == 'X')  {
          warning('Suspect column names (x and y reversed?)')
        }
        if (toupper(substr(cn[1],1,3) == 'LAT' | toupper(substr(cn[2],1,3)) == 'LON'))  {
          warning('Suspect column names (longitude and latitude reversed?)')
        }
      }		
    } else {
      stop('points should be vectors of length 2, matrices with 2 columns, or inheriting from a SpatialPoints* object')
    }
  
  if (! is.numeric(p) ) { p[] <- as.numeric(p) 
  }
  

  return(p)
}


