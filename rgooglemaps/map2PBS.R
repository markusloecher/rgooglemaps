map2PBS = function(xy){
  stopifnot("map" %in% class(xy))
  polys = cbind.data.frame(xy$x, xy$y)
  tmp = rle(rowSums(is.na(polys))==0)
  PolyLengths = tmp$lengths[tmp$values]
  numPolys = length(PolyLengths)
  stopifnot(numPolys == length(xy$names))
  polys2 = na.omit(polys)
  colnames(polys2) = c("X", "Y")
  POS = unlist(sapply(PolyLengths, function(n) 1:n))
  PID = rep(1:numPolys, time = PolyLengths)
  PBSpolys = cbind.data.frame(PID=PID, SID = 1, POS=POS, polys2) 
  
  return(PBSpolys)
}