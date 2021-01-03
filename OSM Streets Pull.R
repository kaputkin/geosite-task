library(osmdata)
library(sp)

#set bounds
n = 31.931529087
s = 31.466378551
w = -104.044406320
e = -103.373271248

#make bounding box
CRS <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
my_box <- rgeos::bbox2SP(n, s, w, e, proj4string = CRS(CRS))
mapview::mapview(my_box)

#pull streets in bounding box
streets <- opq(bbox = c(e, s, w, n)) %>%
  add_osm_feature(key = "highway", value = c("motorway","primary", "secondary", "tertiary", "residential", "track", "service"))
streets <- osmdata_sp(streets, quiet = TRUE)
streets <- streets$osm_lines

#check plot
mapview::mapview(my_box) + mapview::mapview(streets)

#export GEOJSON
setwd("/Users/mac/Desktop/Geosite/Data")
rgdal::writeOGR(streets, "streets", layer="streets", driver="GeoJSON")
