## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
loadLibs = function(libs="RgoogleMaps"){
  for (l in libs) 
    suppressMessages(suppressWarnings(library(l,character.only =TRUE, quietly=TRUE)))
}

loadLibs("RgoogleMaps")
#apiKey = scan("C:/Users/loecherm/Dropbox/stuff/bingAPIkey.txt",what="")

## ----fig.width=6,fig.height=6--------------------------------------------
par(pty="s")
mapBG1 = plotmap("Brandenburg Gate, Berlin", zoom = 15)

mapBG2 = plotmap("Brandenburg Gate, Berlin", zoom = 16, maptype="satellite")

## ----fig.width=6,fig.height=6--------------------------------------------
par(pty="s")

#mapBG3 = GetBingMap(center="Brandenburg Gate, Berlin", zoom=12, extraURL="&mapLayer=TrafficFlow", 
#                      apiKey=apiKey,verbose=0, destfile="BerlinTraffic.png")
mapBG3 = ReadMapTile("BerlinTraffic.png")
PlotOnStaticMap(mapBG3)

## ----fig.width=6,fig.height=6--------------------------------------------
par(pty="s")

ManHatMap <- GetMap(center="Lower Manhattan", zoom=15, 
                      extraURL="&style=feature:road.highway|visibility:off")
  PlotOnStaticMap(ManHatMap)

## ----fig.width=6,fig.height=6--------------------------------------------
data(incidents)
col=as.numeric(incidents$Category)
par(pty="s")

mapSF_Z15 = plotmap(lat, lon, zoom = 15, col = col, pch=20, data = incidents)
#mapSF_Z13 = with(incidents, plotmap(lat, lon, zoom = 13, col = "Category", pch=20))

#lower zoom
mapSF_Z13 = RgoogleMaps::plotmap(lat, lon, zoom = 13, col = col, pch=20, data = incidents, alpha = 0.7)

## ----fig.width=6,fig.height=6--------------------------------------------
par(pty="s")
SundayCrimes = subset(incidents, DayOfWeek=="Sunday")
col=as.numeric(SundayCrimes$violent)
plotmap(lat, lon, mapSF_Z13, col = col, pch=20, data = SundayCrimes, alpha = 0.5)

## ---- fig.width=8, eval=FALSE--------------------------------------------
## par(mfrow=c(1,2),pty="s")
## zoom <- 3
## #Centering at -180:
## lon=c(175,-180,-170, 160)
## markers = cbind.data.frame(label= paste(1:4), lat=65,lon=lon, stringsAsFactors=FALSE)
## 
## terrmap <- GetMap(center=c(65, -180), zoom=zoom, maptype= "terrain",
##                   destfile="terrmap.png",markers=markers,verbose=1)
## 
## 
## 
## lon=c(175-360,-180,-170, 160-360)
## 
## PlotOnStaticMap(terrmap,lat=65,lon=lon,col=1:(length(lon)),pch=20)
## 
## #Centering at 180:
## 
## lon=c(175,-180,-170, 160)
## markers = cbind.data.frame(label= paste(1:4), lat=65,lon=lon, stringsAsFactors=FALSE)
## 
## terrmapb <- GetMap(center=c(65, 180), zoom=zoom, maptype= "terrain",
##                   destfile="terrmapb.png",markers=markers,verbose=1)
## 
## 
## lon=c(175,-180+360,-170+360, 160)
## 
## PlotOnStaticMap(terrmapb,lat=65,lon=lon,col=1:(length(lon)),pch=20)

## ---- fig.width=9, fig.height=9------------------------------------------
suppressWarnings(require(loa, quietly = TRUE))
suppressWarnings(require(latticeExtra, quietly = TRUE))

data(incidents)

cols <- c("yellow", "darkred")
GoogleMap(~lat*lon|DayOfWeek,         #plot conditioned         
          data=incidents, map=NULL,
          groups=incidents$violent,                                               
          col=cols, cex=0.1, alpha=0.3,                 
          panel=panel.loaPlot2,                         
          key.groups=list(main="Violent", 
                          cex=1, col=cols))    

loa_mapSF <- getMapArg()

## ----fig.width=6,fig.height=6--------------------------------------------
v <- ifelse(incidents$violent==TRUE, "Violent", "Non-violent")
#weekend versus weekday:
WE = ifelse(incidents$DayOfWeek %in% c("Saturday", "Sunday"),"weekend", "weekday")

useOuterStrips(GoogleMap(~lat*lon|WE+v,         #plot conditioned         
          data=incidents, map=loa_mapSF,
          panel=panel.kernelDensity,                          #surface plot control                                                                
          col.regions=c("lightyellow", "darkred"),            #
          alpha.regions=0.5,                                  #                       
          col=1,                                              #surface calculation    
          n=200, at=c(0.5,1,2.5,5,10,25,50), 
          scales=list(draw=FALSE), xlab="", ylab=""))     
#useOuterStrips(trellis.last.object())


## ----fig.width=6,fig.height=6--------------------------------------------
# loadLibs("choroplethr")
# options(warn=-1)
# data(df_pop_county)
# 
# choroplethr::county_choropleth(df_pop_county,
#     title         = "2012 California County Population Estimates",
#     legend        = "Population",
#     state_zoom    = "california",
#     reference_map = TRUE)


