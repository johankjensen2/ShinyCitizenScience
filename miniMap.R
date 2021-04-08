library(shiny)
library(leaflet)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  #18n$t("Enter the longitude and latitude of your school. You can find these by right-clicking on Google Maps."),
  HTML("<br/>", "<br/>"), #whitespace,
  numericInput("lat", label = "Latitude:", value = 55.714203),
  numericInput("long", label = "Longitude:", value = 13.207879),
  
  actionButton("showMap", "Show map"),
  HTML("<br/>", "<br/>", "<br/>", "<br/>"), # white space
  
  leafletOutput("mymap", width = 600, height = 400)
  

)

server <- function(input, output, session) {
  
  points <- eventReactive(input$recalc, {
    cbind(input$long, input$lat)
  }, ignoreNULL = FALSE)
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(lat = 56, lng = 11, zoom = 3.49) %>%
      addProviderTiles(provider = "OpenStreetMap.Mapnik",
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircles(data = points(), radius = 15,
                 group = NULL, stroke = TRUE, color = "#03F",
                 weight = 5, opacity = 0.8, fill = TRUE, fillColor = "#03F",
                 fillOpacity = 0.5, label = T)
  })
}

shinyApp(ui, server)