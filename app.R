library(dplyr)
library(htmltools)
library(shiny)
library(sf)
library(leaflet)

###########
# UI      # 
###########

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "80%"),
  uiOutput("age_slider")
  )

###########
# Dataset # 
###########

# Sites
s <- read.csv("s.csv")

# Styles
st <- read.csv("StilGrChrono.Freq.csv")
st <- st_as_sf(st, wkt = "WKT")
st <- st_buffer(st, 0.1)

# Centeroids for styles/labels:
st.cent <- st_centroid(st)
st.cent <- cbind(st.cent,st_coordinates(st.cent))

pal <- colorBin("YlOrRd", domain = st$FROM, reverse = T)

# BantuFirst
bf <- read.csv("bf.csv")
bf<- st_as_sf(bf, wkt = "WKT")

bf <- cbind(bf,st_coordinates(bf))

bf.chrono <- read.csv("bf_chrono.csv")
bf.chrono<- st_as_sf(bf.chrono, wkt = "WKT")
bf.chrono <- cbind(bf.chrono,st_coordinates(bf.chrono))

###########
# Server  # 
###########

server <- function(input, output) {
  
  output$age_slider <- renderUI({
    sliderInput(
      "range", 
      "Age", 
      width = "100%", 
      min = min(st$FROM, na.rm = TRUE),
      max = max(st$TO, na.rm = TRUE),
      step = 100,
      value = c(min(st$FROM), max(st$TO))
    )
  })
  
  output$map <- renderLeaflet({
    leaflet(s, 
            options = leafletOptions(minZoom = 5, 
                                     maxZoom = 11)) %>% 
      addProviderTiles(providers$Stamen.TonerLite, 
                       group = "Toner Lite") %>%
      addProviderTiles(providers$Esri.NatGeoWorldMap, 
                       group = "ESRI") %>%
      addTiles(group = "OpenStreetMap") %>% 
      fitBounds(~min(as.numeric(bf$X)), 
                ~min(as.numeric(bf$Y)), 
                ~max(as.numeric(bf$X)), 
                ~max(as.numeric(bf$Y))) %>% 
      addLayersControl(
        position = "bottomright",
        baseGroups = c("Toner Lite", "ESRI", "OpenStreetMap"),
        overlayGroups = c("BantuFirst sites", "BantuFirst sites (dated)", "Reference Sites", "Pottery Styles"),
        options = layersControlOptions(collapsed = F)) %>%
      hideGroup(c("Pottery Styles", "BantuFirst sites (dated)"))
  })
  
  filteredst <- reactive({
    st[st$TO >= input$range[1] & st$FROM <= input$range[2],]
  })
  
  filteredlb <- reactive({
    st.cent[st.cent$TO >= input$range[1] & st.cent$FROM <= input$range[2],]
  })
  
  filteredbf <- reactive({
    bf.chrono[bf.chrono$TO >= input$range[1] & bf.chrono$FROM <= input$range[2],]
  })
  
  observe({
    leafletProxy("map", data = s) %>% 
      clearControls() %>% 
      clearShapes() %>% 
      clearMarkers() %>% 
      addPolygons(data = filteredst(),
                  stroke = FALSE, 
                  fillColor = ~pal(FROM),
                  popup = ~htmlEscape(Type),
                  group = "Pottery Styles") %>% 
      # TODO: add lables for the styles (code is not reactive like)
      #addLabelOnlyMarkers(data = filteredlb(),
      #                    ~as.numeric(st.cent$X),
      #                    ~as.numeric(st.cent$Y),
      #                    label = ~Type,
      #                    labelOptions = labelOptions(noHide = TRUE, direction = 'top', textOnly = TRUE),
      #                    group = "Pottery Styles") %>%
      addCircleMarkers(data = filteredbf(),
                       popup = ~htmlEscape(name), 
                       color = ~pal(FROM),
                       group = "BantuFirst sites (dated)") %>%
      addCircleMarkers(data = s,
                 ~as.numeric(s$st_x),
                 ~as.numeric(s$st_y), 
                 color = 'grey', 
                 stroke = FALSE, 
                 fillOpacity = 0.5,
                 popup = ~htmlEscape(name),
                 group= "Reference Sites") %>%
      addCircleMarkers(data = bf,
                       ~as.numeric(bf$X),
                       ~as.numeric(bf$Y),
                       popup = ~htmlEscape(name), 
                       color = '#1E64C8',
                       group = "BantuFirst sites") %>%
      addLegend(data = st, 
                pal = pal, 
                values = ~FROM, 
                opacity = 0.7, 
                title = NULL,
                position = "topright", 
                group = "Pottery Styles")
  })
}  

shinyApp(ui, server)
