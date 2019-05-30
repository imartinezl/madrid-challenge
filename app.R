
# Libraries ---------------------------------------------------------------

library(dplyr)
library(zeallot)


# Data Visualization ------------------------------------------------------

#source('data_exploration.R')

# Route Optimization ------------------------------------------------------

source('route_opt.R')

# Shiny -------------------------------------------------------------------

ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("united"),
  shiny::titlePanel("Tabsets"),
  shiny::mainPanel(width = 12,
    shiny::tabsetPanel(
      shiny::tabPanel("Explorar Mapa", leaflet::leafletOutput("mymap", width = "100%", height="800px")),
      shiny::tabPanel("Optimizar Ruta", 
                      shiny::h3("Simulated Annealing Parameters"),
                      shiny::numericInput("num", "S-curve Amplitude", value = 4000),
                      shiny::numericInput("num", "S-curve Center", value = 0),
                      shiny::numericInput("num", "S-curve Width", value = 3000),
                      shiny::numericInput("num", "Total Iterations", value = 25000),
                      shiny::numericInput("num", "Draw Map Every N Iterations", value = 500)
                     )
    )
  )
  #leaflet::leafletOutput("mymap", width = "100%", height="800px")
  
)

server <- function(input, output, session) {
  
  output$mymap <- leaflet::renderLeaflet({
    #data_exploring_map
  })
  
}

shiny::shinyApp(ui, server)
