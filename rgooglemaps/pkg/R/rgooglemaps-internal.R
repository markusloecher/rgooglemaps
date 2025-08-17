.onAttach <- 
  function(libname, pkgname) {
    packageStartupMessage("\nThank you for using RgoogleMaps!")
    packageStartupMessage("\nTo acknowledge our work, please cite the package: \n")
    packageStartupMessage(" Markus Loecher and Karl Ropkins (2015). RgoogleMaps and loa: Unleashing R
  Graphics Power on Map Tiles. Journal of Statistical Software 63(4), 1-18.")
    #packageStartupMessage(" R package version 5.2.3. https://CRAN.R-project.org/package=stargazer \n")
  }