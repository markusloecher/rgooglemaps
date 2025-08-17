getGeoCode <- structure(function#geocoding utility
### Geocode your data using, R, JSON and OSM or Google Maps' Geocoding APIs 
(
  gcStr, ##<< adddress to geocode
  API = c("osm", "google")[1], ##<< which API to use. see https://nominatim.org/release-docs/develop/api/Search/ and http://allthingsr.blogspot.de/2012/01/geocode-your-data-using-r-json-and.html
  JSON = FALSE, ##<< use the JSON protocol. If FALSE, we do not have to load additional libraries
  verbose=0 ##<< level of verbosity
){
  #library("RJSONIO") #Load Library
  gcStr <- enc2utf8(gsub(' ','%20',gcStr)) #Encode URL Parameters
  if (API == "google"){
    #baseURL= paste0('http://maps.google.com/maps/api/geocode/",ifelse(JSON, "json","xml"),"?sensor=false&address=')
  
    if (JSON){
      #Open Connection
      # connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&address=',gcStr, sep="") 
      # if (verbose) cat("fetching ", connectStr, "\n")
      # con <- url(connectStr)
      # data.json <- fromJSON(paste(readLines(con), collapse=""))
      # close(con)
      # #Flatten the received JSON
      # data.json <- unlist(data.json)
      # lat <- data.json["results.geometry.location.lat"]
      # lng <- data.json["results.geometry.location.lng"]
      
      connectStr <- paste('http://maps.google.com/maps/api/geocode/json?sensor=false&address=',gcStr, sep="") 
      if (verbose) cat("fetching ", connectStr, "\n")
      con <- url(connectStr)
      data.json <- readLines(con)
      close(con)
      
      iLoc = grep("\"location\"", data.json,fixed=TRUE)
      iLat = grep("\"lat\"", data.json,fixed=TRUE); iLat = min(iLat[iLat>iLoc])
      lat = as.numeric(gsub(",", "",gsub("\"lat\" : ", "", data.json[iLat])))
      iLng = grep("\"lng\"", data.json,fixed=TRUE); iLng = min(iLng[iLng>iLoc])
      lng = as.numeric(gsub(",", "",gsub("\"lng\" :", "", data.json[iLng])))
      
    } else {
      #Open Connection
      connectStr <- paste('http://maps.google.com/maps/api/geocode/xml?sensor=false&address=',gcStr, sep="") 
      if (verbose) cat("fetching ", connectStr, "\n")
      con <- url(connectStr)
      data.xml <- readLines(con)
      close(con)
      #browser()
      iLoc = grep("<location>", data.xml,fixed=TRUE)
      iLat = grep("<lat>", data.xml,fixed=TRUE); iLat = min(iLat[iLat>iLoc])
      lat = as.numeric(gsub("</lat>", "",gsub("<lat>", "", data.xml[iLat])))
      iLng = grep("<lng>", data.xml,fixed=TRUE); iLng = min(iLng[iLng>iLoc])
      lng = as.numeric(gsub("</lng>", "",gsub("<lng>", "", data.xml[iLng])))
    }
  } else if (API == "osm") {
    #baseURL= paste0("https://nominatim.openstreetmap.org/search?format=",ifelse(JSON, "json","xml"),"&polygon=0&addressdetails=0&q=")
    # https://nominatim.openstreetmap.org/search?format=xml&polygon=0&addressdetails=0&q=135+pilkington+avenue,+birmingham&
    #https://nominatim.openstreetmap.org/search?q=17+Strada+Pictor+Alexandru+Romano%2C+Bukarest&format=xml
    if (JSON) {
      
    } else {
      connectStr <- paste0("https://nominatim.openstreetmap.org/search?q=",gcStr,"&format=xml&limit=1") 
      if (verbose) cat("fetching ", connectStr, "\n")
      con <- url(connectStr)
      data.xml <- readLines(con, warn=FALSE)
      close(con)
      #browser()
      tmp=unlist(strsplit( data.xml," "))
      iLat = grep("lat=", tmp,fixed=TRUE)
      lat = as.numeric(gsub("lat='|'", "",tmp[iLat]))
      iLng = grep("lon=", tmp,fixed=TRUE)
      lng = as.numeric(gsub("lon='|'", "",tmp[iLng]))
    }
    
  }
  gcodes <- as.numeric(c(lat, lng))
  names(gcodes) <- c("lat", "lon")
  return (gcodes)
### returns lat/lon for address
}, ex = function(){
  if (0){
    getGeoCode("1600 Amphitheatre Parkway, Mountain View, CA")
    getGeoCode("Brooklyn")
    #You can run this on the entire column of a data frame or a data table:
    DF = cbind.data.frame(address=c("Berlin,Germany", "Princeton,NJ", 
              "cadillac+mountain+acadia+national+park"), lat = NA, lon = NA)
    DF <- with(DF, data.frame(address, t(sapply(DF$address, getGeoCode))))
  }
})


