
#Create base map====
?addProviderTiles

basemap <- leaflet() %>%
  # add different provider tiles
  addProviderTiles(
    "Stamen.Terrain",
    # give the layer a name
    group = "Stamen.Terrain"
  ) %>%
  addProviderTiles(
    "OpenTopoMap",
    # give the layer a name
    group = "OpenTopoMap"
  ) %>%
  addProviderTiles(
    "Esri.WorldImagery",
    # give the layer a name
    group = "Esri.WorldImagery"
  ) %>%
  # add a layers control
  addLayersControl(
    baseGroups = c(
      "Stamen.Terrain","OpenTopoMap","Esri.WorldImagery"
    ),
    # position it on the topleft
    position = "topleft"
  )


basemap

icon.fa <- makeAwesomeIcon(
  icon = "flag", markerColor = "red",
  library = "fa",
  iconColor = "black"
)

map_1 <- basemap %>%
  addAwesomeMarkers(
    lat = 69.13,
    lng = -105.06,
    label = "Sampling point",
    icon = icon.fa
  )

map_1

#69+07/60+48/3600 69.13
#105+03/60+36/3600 105.06

map_3 <- basemap %>%
  # add pharmacies
  addMarkers(
    data = pharmacies_inter,
    # create custom labels
    label = paste(
      "Name: ", pharmacies_inter$name, "<br>",
      "Distance from location: ",
      round(pharmacies_inter$distance, 1), " meters", "<br>",
      "Street: ", pharmacies_inter$addr_st
    ) %>%
      lapply(htmltools::HTML)
  ) %>%
  # add a legend
  addLegend(
    colors = "#E84A5F",
    labels = "0 - 20 minutes",
    title = "Drivetime",
    opacity = 1, 
    position = "bottomleft"
  )
