library(shiny)
library(leaflet)

###########
# UI      # 
###########

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  textOutput("value"), 
  absolutePanel(top = 10, right = 10,
                selectInput(inputId = "color", 
                            label = "Chose a Value:",
                            choices = c("Branch" = 'pal.branch', 
                                        "Guthrie Index" = 'pal.guthrie'))
  )
)

###########
# Dataset # 
###########

source("conf.R")
d <- dbGetQuery(con, 
"SELECT
  ST_X(ST_AsEWKT(geom)) AS long, 
  ST_Y(ST_AsEWKT(geom)) AS lat, 
  ST_AsText(geom) AS Koordinaten
FROM geodata;")


###########
# Server  # 
###########

server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet(d, 
            options = leafletOptions(minZoom = 5, 
                                     maxZoom = 11)) %>% 
      addProviderTiles(providers$Stamen.TonerLite, 
                       group = "Toner Lite") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, 
                       group = "ESRI") %>%
      addTiles(group = "OpenStreetMap") %>% 
      fitBounds(~min(as.numeric(d$long)), 
                ~min(as.numeric(d$lat)), 
                ~max(as.numeric(d$long)), 
                ~max(as.numeric(d$lat))) %>% 
      addLayersControl(
        position = "bottomright",
        baseGroups = c("Toner Lite", "ESRI", "OpenStreetMap"),
        options = layersControlOptions(collapsed = T)) %>%
      addCircleMarkers(data = d,
                       ~as.numeric(d$long),
                       ~as.numeric(d$lat),
                       fillOpacity = .5)
  })
  
}

shinyApp(ui, server)
  