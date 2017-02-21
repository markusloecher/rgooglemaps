## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ---- eval = TRUE--------------------------------------------------------
library(RgoogleMaps)
(center=getGeoCode("Washington Square Park;NY"))
GetMapTiles(center, zoom=16,nTiles = c(20,20))
  

## ---- eval = TRUE--------------------------------------------------------
for (zoom in 13:15)
  GetMapTiles("Washington Square Park;NY", zoom=zoom,nTiles = round(c(20,20)/(17-zoom)))
  

## ---- eval = TRUE,fig.width=10,fig.height=10-----------------------------
for (zoom in 13:16)
  GetMapTiles("Washington Square Park;NY", zoom=zoom,nTiles = round(c(20,20)/(17-zoom)),
              urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir= "~/mapTiles/Google/")

#just get 3x3 tiles:

#mt= GetMapTiles(center = c(lat = 40.73082, lon =-73.99733), zoom=16,nTiles = c(3,3), urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir= "~/mapTiles/Google/", returnTiles = TRUE)

mt= GetMapTiles("Washington Square Park;NY", zoom=16,nTiles = c(3,3),
              urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir= "~/mapTiles/Google/", returnTiles = TRUE)
PlotOnMapTiles(mt)

## ---- eval=FALSE---------------------------------------------------------
## library(leaflet)
##   m = leaflet::leaflet() %>%
##     addTiles( urlTemplate = "http:/localhost:8000/mapTiles/OSM/{z}_{x}_{y}.png")
##   m = m %>% leaflet::setView(-73.99733, 40.73082 , zoom = 16)
##   m = m %>% leaflet::addMarkers(-73.99733, 40.73082 )
##   m

## ---- echo=FALSE ,fig.width=8,fig.height=8-------------------------------
# R/R-3.3.0/bin/i386/Rscript.exe -e 'servr::httd()' -p8000
library(leaflet)
  m = leaflet::leaflet() %>%  addTiles()
  m = m %>% leaflet::setView(-73.99733, 40.73082 , zoom = 16)
  m = m %>% leaflet::addMarkers(-73.99733, 40.73082 )
  m

## ---- eval=FALSE---------------------------------------------------------
## library(leaflet)
##   m = leaflet::leaflet() %>%
##     addTiles( urlTemplate = "http:/localhost:8000/mapTiles/Google/{z}_{x}_{y}.png")
##   m = m %>% leaflet::setView(-73.99733, 40.73082 , zoom = 16)
##   m = m %>% leaflet::addMarkers(-73.99733, 40.73082 )
##   m

## ---- echo=FALSE,fig.width=8,fig.height=8--------------------------------
library(leaflet)
  m = leaflet::leaflet() %>% 
    addTiles( urlTemplate = "http://mt1.google.com/vt/lyrs=m&x={x}&y={y}&z={z}")
  m = m %>% leaflet::setView(-73.99733, 40.73082 , zoom = 16)
  m = m %>% leaflet::addMarkers(-73.99733, 40.73082 )
  m

