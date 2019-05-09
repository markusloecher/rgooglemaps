Google has recently changed its API requirements, and users are now required to register with Google.

You can do this at <https://cloud.google.com/maps-platform/>. The API key requires a valid credit card which makes me very uncomfortable. So instead we have a workaround using map tiles:

``` r
library(RgoogleMaps)

lat = c(40.702147,40.718217,40.711614);
lon = c(-74.012318,-74.015794,-73.998284);
center = c(mean(lat), mean(lon));
zoom <- min(MaxZoom(range(lat), range(lon)));
bb=qbbox(lat,lon)
```

#### Openstreet map server

``` r

par(pty="s")
#OSM
myMap=GetMap(center,zoom=15)
PlotOnStaticMap(myMap,lat=lat,lon=lon,pch=20,col=c('red', 'blue', 'green'),cex=2)
```

![](tools/README-unnamed-chunk-3-1.png)

#### Google map server

``` r
par(pty="s")
#Google
myMap=GetMap(center,zoom=15,urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir= "~/mapTiles/Google/")
PlotOnStaticMap(myMap,lat=lat,lon=lon,pch=20,col=c('red', 'blue', 'green'),cex=2)
```

![](tools/README-unnamed-chunk-4-1.png)

Note that for each geographic region there is a one-time only download of map tiles that are needed to "stitch" together the static map. Afterwards, repeated calls simply access the stored offline map tiles. (When you change the zoom level or the locations, you will download more tiles.)

Compare with previously downloaded maps
---------------------------------------

``` r
#Washington DC

par(pty="s")

mymarkers = cbind.data.frame(char = c("","",""),
                             lat = c(38.898648,38.889112, 38.880940),
                             lon = c(-77.037692, -77.050273, -77.03660), 
                             col = c("blue", "green", "red"))

#get the bounding box:
bb <- qbbox(lat = mymarkers[,"lat"], lon = mymarkers[,"lon"])
#download the map:
mapDC <- GetMap.bbox(bb$lonR, bb$latR)
mapDC2 <- GetMap.bbox(bb$lonR, bb$latR,destfile = "DC1_google.png",urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir= "~/mapTiles/Google/")

PlotOnStaticMap(mapDC2,lat = mymarkers[,"lat"], 
                lon = mymarkers[,"lon"],cex=1.5,pch=20,
                col=c("blue", "green", "red"))
```

![](tools/README-unnamed-chunk-5-1.png)
