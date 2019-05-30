
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
  shiny::titlePanel("Madrid Route Optimization Challenge"),
  shiny::mainPanel(width = 12,
                   shiny::tabsetPanel(selected = "Optimize Route",
                                      shiny::tabPanel("Explore Map", leaflet::leafletOutput("mymap", width = "100%", height="800px")),
                                      shiny::tabPanel("Optimize Route", 
                                                      shiny::column(4,
                                                                    shiny::h3("Simulated Annealing Parameters"),
                                                                    shiny::numericInput("s_curve_amplitude", "S-curve Amplitude", value = 4000, step = 500, min = 0),
                                                                    shiny::numericInput("s_curve_center", "S-curve Center", value = 0, step = 500, min = 0),
                                                                    shiny::numericInput("s_curve_width", "S-curve Width", value = 3000, step=500, min = 1),
                                                                    shiny::numericInput("total_iterations", "Total Iterations", value = 25000, step = 1000, min=1000),
                                                                    shiny::numericInput("plot_every_iterations", "Draw Map Every N Iterations", value = 500, step = 500, min = 500),
                                                                    shiny::actionButton("solve", "Solve!")
                                                      ),
                                                      shiny::column(8,
                                                                    shiny::column(12,shiny::plotOutput("map")),
                                                                    shiny::column(12,shiny::plotOutput("scurve"))
                                                                    
                                                      )
                                      )
                   )
  )
  #leaflet::leafletOutput("mymap", width = "100%", height="800px")
  
)

server <- function(input, output, session) {
  
  object <- shiny::reactiveValues()
  object$iter <- 0
  output$mymap <- leaflet::renderLeaflet({
    #data_exploring_map
  })
  shiny::observeEvent(input$solve, {
  })
  output$scurve <- shiny::renderPlot({
    x <- seq(1, input$total_iterations, length.out = 100)
    #y <- current_temperature(x, 4000, 0, 3000)
    y <- current_temperature(x,input$s_curve_amplitude, input$s_curve_center, input$s_curve_width)
    data.frame(x,y) %>% 
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x=x, y=y))+
      ggplot2::geom_point(x=object$iter, y=current_temperature(object$iter, 4000, 0, 3000), color="red")+
      # ggplot2::geom_segment(x=input$total_iterations, xend=input$total_iterations, y=0, yend=input$s_curve_amplitude/2, 
      #                       size=0.3,  linetype="dashed", color="red",
      #                       arrow = grid::arrow(angle=90, ends="both", length = grid::unit(0.1, "inches")) )+
      # ggplot2::geom_label(x=input$total_iterations, y=input$s_curve_amplitude/2, label="Amplitude/2", angle=90, color="red")+
      # ggplot2::geom_vline(xintercept = input$s_curve_center, linetype="dashed", color="blue")+
      # ggplot2::geom_label(x=input$s_curve_center, y=0, label="Center", angle=90, color="blue")+
      ggplot2::xlab("Iteration")+
      ggplot2::ylab("Temperature")+
      ggplot2::ggtitle("Annealing Schedule")
    
    #plot(x,y, type='l', xlab="Iteration", ylab="Temperature", main = "Annealing Schedule")
    #points(5000, current_temperature(5000, 4000, 0, 3000), pch=21, col="red", bg='red')
  })
  
}

shiny::shinyApp(ui, server)
