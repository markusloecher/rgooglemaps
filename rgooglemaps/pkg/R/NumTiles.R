NumTiles <- structure(function#computes the necessary number of tiles from a bounding box and a zoom level
### computes the necessary number of tiles from a bounding box and a zoom level
(  lonR, ##<< longitude range
   latR, ##<< latitude range, 
   zoom =13 ##<< zoom level
){
  nTiles=c(0,0)
  if (!missing(lonR) & !missing(latR)) {
    XYmin = LatLon2XY(lat=latR[1], lon=lonR[1],zoom=zoom)
    XYmax = LatLon2XY(lat=latR[2], lon=lonR[2],zoom=zoom)
    nTiles[1] = abs(XYmax$Tile[1,1]-XYmin$Tile[1,1])+1
    nTiles[2] = abs(XYmax$Tile[1,2]-XYmin$Tile[1,2])+1
  }
  return(nTiles) 
  ### tuple with number of tiles for lon and lat extent
}, ex = function(){
  #US bounding box:
  NumTiles(lonR=c(-135,-66), latR=c(25,54) , zoom=8)
  
})