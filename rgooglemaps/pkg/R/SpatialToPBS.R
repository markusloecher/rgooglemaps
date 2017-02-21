SpatialToPBS <- structure(function#converts spatial objects as defined in package sp to simpler PBSmapping type dataframes 
### The PlotPolysOnStaticMap() function currently does not take sp objects directly but instead needs
### PBSmapping type data.frames. This function converts sp objects into such.                         
### THANKS TO Fabio Priuli for a major bug fix w.r.t. holes in spatial polygons!
(
  xy, ##<< spatial object, such as SpatialPoints, SpatialPolygons, etc..
  verbose=0 ##<< level of verbosity
  ) {
    fun = points
    if (inherits(xy, "Spatial")) {
      b = sp::bbox(xy)
      if (inherits(xy, "SpatialPoints")) {
        xy = sp::coordinates(xy)
      } else if (inherits(xy, "SpatialPolygons")) {
        res = matrix(ncol = 5, nrow = 0, dimnames = list(NULL, c("PID", "SID", "POS", "X", "Y")))
        for (i in 1:length(xy@polygons)) {
          x = slot(xy@polygons[[i]], "Polygons")
          isHole = unlist(lapply(x, function(x) slot(x, "hole")))
          x = lapply(x, function(x) slot(x, "coords"))
          for (j in 1:length(x)) {
            n = nrow(x[[j]])
            order = 1:n
            if (isHole[j])
              order = n:1
            res = rbind(res, cbind(PID = rep(i, n), SID=rep(j, n), POS = order, x[[j]]))
          }
        }
        xy = res
        fun = lines
        attr(xy, "projection") <- "LL"
      } else if (inherits(xy, "SpatialLines")) {
        res = list()
        for (j in 1:length(xy)){
          res[[j]] = xy@lines[[j]]@Lines[[1]]@coords
        }
        xy = res
        fun = lines
        attr(xy, "projection") <- "LL"
      }  
    } else {
      b = rbind(range(xy[, 1], na.rm = TRUE), range(xy[, 2], na.rm = TRUE))
    }
    return(list(xy = xy, bb = b, fun = fun))
### list with elements xy = converted object, bb = bounding box, fun = plot function
}, ex = function(){
  if (interactive()) {
  data("NYleukemia", envir = environment())
  population <- NYleukemia$data$population
  cases <- NYleukemia$data$cases
  mapNY <- GetMap(center=c(lat=42.67456,lon=-76.00365), 
                  destfile = file.path(tempdir(),"NYstate.png"), 
                  maptype = "mobile", zoom=9)
  #mapNY=ReadMapTile("NYstate.png")
  clrStuff=ColorMap(100*cases/population, alpha = 0.35, log = TRUE)
  NYpolys = SpatialToPBS(NYleukemia$spatial.polygon)
  PlotPolysOnStaticMap(mapNY, NYpolys$xy, col = clrStuff$colcode, add = FALSE)
  legend("topleft", legend = clrStuff$legend, fill = clrStuff$fill, 
         bg = rgb(0.1,0.1,0.1,0.3))
}
  
})
