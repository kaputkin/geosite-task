library(leaflet)
library(leaflet.extras2)
library(dplyr)
library(raster)
library(geojsonio)
library(sf)

# Add data
ghgsat <- raster("C:/Users/Ari/Desktop/Geosite/data (1)/data/GHGSat/GDSW1_SON1Dz0Xvv200717_CON0018000483_COLN01_TIFF/GDSW1_SON1Dz0Xvv200717_CON0018000483_COLN01_TIFF_L2_F32.tiff")
plumes <- geojson_read("C:/Users/Ari/Desktop/Geosite/Data/plume polygons.geojson",  what = "sp")
plume.buffers <- geojson_read("C:/Users/Ari/Desktop/Geosite/Data/plume buffers 500ft.geojson",  what = "sp")
wells <- geojson_read("C:/Users/Ari/Desktop/Geosite/Geosite/Data/4326/wells.geojson",  what = "sp")
hex <- geojson_read("C:/Users/Ari/Desktop/Geosite/Geosite/Data/4326/hexgrid.geojson",  what = "sp")
parcels <- geojson_read("C:/Users/Ari/Desktop/Geosite/Geosite/Data/4326/Parcels.geojson",  what = "sp")



wells$quantile <- ntile(wells$X_median, 5)
hex$quantile <- ntile(hex$X_median, 5)

wells$quantile.text <-  ifelse(wells$quantile == 1,
                   "Low",
                   ifelse(wells$quantile == 2,
                          "Medium Low",
                          ifelse(wells$quantile == 3,
                                 "Medium",
                                 ifelse(wells$quantile == 4,
                                        "Medium High",
                                        "High"))))

plumes <- st_as_sf(plumes)
plume.buffers <- st_as_sf(plume.buffers)
wells <- st_as_sf(wells)
wells <- as(wells, 'Spatial')
rgdal::writeOGR(wells, "wells.quantile", layer="wells", driver="GeoJSON")


#WELL LABELS
wells.labels <- sprintf(
  "<style>
strong {color: #5CD4EF;}
k    {color: #fcfcfc;}
navigate  {font-weight: bold}
  </style>
  <strong>Well ID:</strong> <k> %s </k> <br/>
  <strong>Methane Leak Risk:</strong> <k> %s </k> <br/>
  <strong>Leak Date:</strong> <k> %s </k> <br/> 
  <navigate> <a href= 'url'>NAVIGATE HERE</a> </navigate>",
  wells$API, wells$quantile.text, wells$Date 
) %>% lapply(htmltools::HTML)

parcels.labels <- 
  sprintf(
    "<style>
strong {color: #5CD4EF;}
k    {color: #fcfcfc;}
navigate  {font-weight: bold}
  </style>
  <strong>Property Owner:</strong> <k> %s </k> <br/>
  <strong>Address:</strong> <k> %s </k> <br/>",
    parcels$owner_name, parcels$mail_addr
  ) %>% lapply(htmltools::HTML)

#COLOR SCHEMES
pal <- colorNumeric(
  palette = "Reds",
  domain = wells$quantile)
  
pal.hex <- colorNumeric(
  palette = "Reds",
  domain = hex$quantile,
  na.color=rgb(0,0,0,0))

#"C:/Users/Ari/Desktop/Geosite/Geosite/Vectors/well1.png"
#/Users/mac/Desktop/Geosite/Vectors/well5.png

#ICONS
leafIcons <- icons(
  iconUrl = ifelse(wells$quantile == 1,
                   "C:/Users/Ari/Desktop/Geosite/Geosite/Vectors/well1v3.png",
            ifelse(wells$quantile == 2,
                   "C:/Users/Ari/Desktop/Geosite/Geosite/Vectors/well2v3.png",
            ifelse(wells$quantile == 3,
                   "C:/Users/Ari/Desktop/Geosite/Geosite/Vectors/well3v3.png",
            ifelse(wells$quantile == 4,
                   "C:/Users/Ari/Desktop/Geosite/Geosite/Vectors/well4v3.png",
                   "C:/Users/Ari/Desktop/Geosite/Geosite/Vectors/well5v4.png")))),
  iconWidth = 18, iconHeight = 35
)

#BUILD MAP  
leaflet() %>%
  setView(-103.72334,31.67214,  zoom = 11) %>%
  addTiles("https://kaputkin.github.io/tileserver/tiles/Washed_Sat/{z}/{x}/{y}.png", options = tileOptions(minZoom=9, maxZoom=17)) %>%
  addProviderTiles(providers$Stamen.TonerHybrid, options = providerTileOptions(opacity = 0.65, minZoom = 9, maxZoom = 17)) %>%
  
#ADD WELL ICONS
  addMarkers(data = wells, 
             icon = leafIcons,
             popup = wells.labels,
             popupOptions = labelOptions(
               style = list("font-weight" = "normal", padding = "3px 8px"),
               textsize = "15px",
               direction = "auto"),
             group = "Oil Wells")%>%
  groupOptions("Oil Wells", zoomLevels = 14:17)%>%  

#ADD WELL MARKERS
  
addCircleMarkers(data = wells,
                 fillColor = ~pal(wells$quantile),
                 radius = 3.5,
                 stroke = T,
                 weight = 2,
                 color= 'white',
                 fillOpacity = 1,
                 popup = wells.labels,
                 popupOptions = labelOptions(
                   style = list("font-weight" = "normal", padding = "3px 8px"),
                   textsize = "15px",
                   direction = "auto"),
                 group = "wells.markers")%>%
groupOptions("wells.markers", zoomLevels = 12:13)%>%
  
addCircles(data = wells,
           fillColor = ~pal(wells$quantile),
           radius = 103.5,
           stroke = F,
           weight = 2,
           color= 'white',
           fillOpacity = 0,
           highlightOptions = highlightOptions(
             weight = 3,
             stroke = T,
             color = "red"),
           popup = wells.labels,
           popupOptions = labelOptions(
             style = list("font-weight" = "normal", padding = "3px 8px"),
             textsize = "15px",
             direction = "auto"),
           group = "wells.markers")%>%
  groupOptions("Wells.markers", zoomLevels = 12:13)%>%


#ADD HEX
addPolygons(data = hex, 
            fillColor = ~pal.hex(hex$quantile),
            weight = .5,
            opacity = .7,
            color = "white",
            fillOpacity = 0.7,
            group = "Hex")%>%
  groupOptions("Hex", zoomLevels = 1:11)%>%
  
  
#ADD PARCELS
addPolygons(data = parcels,
           weight = .4,
           opacity = 1,
           color = "white",
           fillOpacity = 0,
           highlightOptions = highlightOptions(
             weight = 6,
             color = "blue",
             opacity = 1,
             bringToFront = TRUE),
           popup = parcels.labels,
           popupOptions = labelOptions(
             style = list("font-weight" = "normal", padding = "3px 8px"),
             textsize = "15px",
             direction = "auto"),
           group = "Property Parcels")%>%
  
addTiles("https://kaputkin.github.io/tileserver/tiles/raster-shadow/{z}/{x}/{y}.png", options = tileOptions(minZoom=12, maxZoom=17), group = "Methane Concentration (Colors #1)")%>%
addTiles("https://kaputkin.github.io/tileserver/tiles/raster-shadow2/{z}/{x}/{y}.png", options = tileOptions(minZoom=12, maxZoom=17), group = "Methane Concentration (Colors #2)")%>%

hideGroup("Property Parcels")%>%
hideGroup("Methane Concentration (Colors #1)")%>%


addLegend(values = wells$quantile, group = "wells.markers", title = "Oil Well Leak Probability", position = "bottomleft", labels = "1", pal=pal)%>%
addLayersControl(baseGroups = c("Methane Concentration (Colors #1)", 
                                  "Methane Concentration (Colors #2)"),
                 overlayGroups= c("Property Parcels", "Oil Wells"), 
                   options = layersControlOptions(collapsed = FALSE))
  
addLayersControl(overlayGroups= c("Property Parcels", "Oil Wells"),  options = layersControlOptions(collapsed = FALSE))




