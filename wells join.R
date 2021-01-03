library(raster)
library(rgdal)
library(rgeos)
library(dplyr)
library(sf)
library(geojsonio)

ghgsat <- raster("C:/Users/Ari/Desktop/Geosite/data (1)/data/GHGSat/GDSW1_SON1Dz0Xvv200717_CON0018000483_COLN01_TIFF/GDSW1_SON1Dz0Xvv200717_CON0018000483_COLN01_TIFF_L2_F32.tiff")
plumes <- geojson_read("C:/Users/Ari/Desktop/Geosite/Data/plume polygons.geojson",  what = "sp")
plume.buffers <- geojson_read("C:/Users/Ari/Desktop/Geosite/Data/plume buffers 500ft.geojson",  what = "sp")
wells <- geojson_read("C:/Users/Ari/Desktop/Geosite/Data/wells.geojson",  what = "sp")

plumes <- st_as_sf(plumes)
plume.buffers <- st_as_sf(plume.buffers)
wells <- st_as_sf(wells)

wells.join <- st_join(wells, plume.buffers, st_intersects, left = T)

#take only top ppb quantile for each well
wells.join$X_median[is.na(wells.join$X_median)] <- 0
wells.join <- wells.join %>% group_by(fid.x) %>% top_n(1, X_median)



geojson_write(wells.join,'C:/Users/Ari/Desktop/Geosite/Data/wells_join.geojson')

getwd()

writeOGR(wells.join, "wells.join", layer="wells.join", driver="GeoJSON")