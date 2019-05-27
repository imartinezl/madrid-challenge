
# Libraries ---------------------------------------------------------------

library(dplyr)
library(zeallot)


# Data Visualization ------------------------------------------------------

source('data_exploration.R')

# Route Optimization ------------------------------------------------------

source('route_opt.R')

# Shiny -------------------------------------------------------------------

ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("united"),
  leaflet::leafletOutput("mymap")
  
)

server <- function(input, output, session) {
  
  output$mymap <- leaflet::renderLeaflet({
    data_exploring_map
  })
  
}

shiny::shinyApp(ui, server)
