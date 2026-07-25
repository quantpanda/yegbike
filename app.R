#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(dplyr)

# ----------------------------
# Shiny App
# ----------------------------
ui <- fluidPage(
  titlePanel("Edmonton Bike Lanes Map"),
  leafletOutput("bikeMap", height = 600)
)

server <- function(input, output, session) {
  
  output$bikeMap <- renderLeaflet({
    # Use colorFactor for categorical variable
    qpal <- leaflet::colorFactor(
      palette = "Dark2",
      domain = bike_routes$classification,
      na.color = "grey"
    )
    
    labss <- lapply(1:nrow(bike_routes), function(i) {
      shiny::HTML(paste(
        "Street: ", bike_routes$street_name_full[i], "<br>",
        "Type: ", bike_routes$type[i], "<br>",
        "Classification: ", bike_routes$classification[i], "<br>"
      ))
    })
    
    leaflet(bike_routes) %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addPolylines(
        color = ~qpal(classification),
        weight = 2,
        opacity = 1,
        label = labss,
        highlightOptions = leaflet::highlightOptions(
          weight = 5,
          color = "#000",
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        "topright",
        pal = qpal,
        values = ~classification,
        title = "Bike Lane Classification",
        opacity = 1
      ) %>%
      addScaleBar(position = "bottomleft")
  })
  
}
# call global.R to load data
source("global.R")
shinyApp(ui, server)
