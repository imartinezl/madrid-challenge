
# Libraries ---------------------------------------------------------------

library(dplyr)
library(zeallot)


# Data Visualization ------------------------------------------------------

source('data_exploration.R')

# Route Optimization ------------------------------------------------------

source('route_opt.R')

ensure_between = function(num, min_allowed, max_allowed) {
  max(min(num, max_allowed), min_allowed)
}


# Shiny -------------------------------------------------------------------

ui <- shiny::fluidPage(
  theme = shinythemes::shinytheme("united"),
  shiny::titlePanel("Madrid Route Optimization Challenge"),
  shiny::mainPanel(width = 12,
                   shiny::tabsetPanel(selected = "Optimize Route",
                                      shiny::tabPanel("Explore Map", 
                                                      shinycssloaders::withSpinner(
                                                        leaflet::leafletOutput("mymap", width = "100%", height="800px"), type=4,color = "#9598a0")
                                      ),
                                      shiny::tabPanel("Optimize Route", 
                                                      shiny::column(3,
                                                                    shiny::h3("Simulated Annealing Parameters"),
                                                                    shiny::numericInput("s_curve_amplitude", "S-curve Amplitude", value = 4000, step = 500, min = 0, width = "35%"),
                                                                    shiny::numericInput("s_curve_center", "S-curve Center", value = 0, step = 500, min = 0, width = "35%"),
                                                                    shiny::numericInput("s_curve_width", "S-curve Width", value = 3000, step = 500, min = 1, width = "35%"),
                                                                    shiny::numericInput("total_iterations", "Total Iterations", value = 25000, step = 1000, min=1000, width = "35%"),
                                                                    shiny::numericInput("plot_every_iterations", "Draw Map Every", value = 500, step = 500, min = 500, width = "35%"),
                                                                    shiny::h3("Optimization Parameters"),
                                                                    shiny::selectInput("optvar", "Optimize", choices = c("traffic_time","distance"), multiple = F, width = "35%"),
                                                                    shiny::actionButton("solve", "Solve!", width = "35%")
                                                                    
                                                                    
                                                      ),
                                                      shiny::column(9,
                                                                    shiny::column(4,
                                                                                  shiny::plotOutput("scurve", height= "400px"),
                                                                                  shiny::plotOutput("distance", height= "400px")
                                                                                  ),
                                                                    shiny::column(8,
                                                                                  # shiny::plotOutput("route", height = "800px")
                                                                                  leaflet::leafletOutput("routemap", height = "800px")
                                                                                  )
                                                      
                                                      )
                                      )
                   )
  )

)

#https://github.com/toddwschneider/shiny-salesman
server <- function(input, output, session) {
  
  vals <- shiny::reactiveValues(iter=0)
  output$mymap <- leaflet::renderLeaflet({
    data_exploring_map
  })
  setup_to_run_annealing_process = observe({
    input$solve

    shiny::isolate({
      vals$tour <- sample(nrow(points))
      vals$tour_distance <- calculate_tour_distance(vals$tour, distance.matrix(edges_summary, input$optvar))
      vals$best_tour <- c()
      vals$best_distance <- Inf
      
      vals$s_curve_amplitude <- ensure_between(input$s_curve_amplitude, 0, 1000000)
      vals$s_curve_center <- ensure_between(input$s_curve_center, -1000000, 1000000)
      vals$s_curve_width <- ensure_between(input$s_curve_width, 1, 1000000)
      vals$total_iterations <- ensure_between(input$total_iterations, 1, 1000000)
      vals$plot_every_iterations <- ensure_between(input$plot_every_iterations, 1, 1000000)
      
      vals$number_of_loops <- ceiling(vals$total_iterations / vals$plot_every_iterations)
      vals$distances <- rep(NA, vals$number_of_loops)
      
      vals$iter <- 0
    })
    
    run_annealing_process$resume()
  }, priority=20)
  
  run_annealing_process = observe({
    qry = shiny::parseQueryString(session$clientData$url_search)
    if (input$solve == 0 & is.null(qry$auto)) return()

    if (nrow(isolate(points)) < 2) return()
    
    isolate({
      intermediate_results = run_intermediate_annealing_process(
        points = points,
        distance_matrix = distance.matrix(edges_summary, input$optvar),
        tour = vals$tour,
        tour_distance = vals$tour_distance,
        best_tour = vals$best_tour,
        best_distance = vals$best_distance,
        starting_iteration = vals$iter,
        number_of_iterations = vals$plot_every_iterations,
        s_curve_amplitude = vals$s_curve_amplitude,
        s_curve_center = vals$s_curve_center,
        s_curve_width = vals$s_curve_width
      )
      
      vals$tour = intermediate_results$tour
      vals$tour_distance = intermediate_results$tour_distance
      vals$best_tour = intermediate_results$best_tour
      vals$best_distance = intermediate_results$best_distance
      
      vals$iter = vals$iter + vals$plot_every_iterations
      
      vals$distances[ceiling(vals$iter / vals$plot_every_iterations)] = intermediate_results$tour_distance
    })
    
    if (shiny::isolate(vals$iter) < shiny::isolate(vals$total_iterations)) {
      invalidateLater(0, session)
    } else {
      isolate({
        vals$tour = vals$best_tour
        vals$tour_distance = vals$best_distance
      })
    }
  }, priority=10)
  
  output$scurve <- shiny::renderPlot({
    x <- seq(1, input$total_iterations, length.out = 100)
    y <- current_temperature(x,input$s_curve_amplitude, input$s_curve_center, input$s_curve_width)
    data.frame(x,y) %>% 
      ggplot2::ggplot() +
      ggplot2::geom_line(ggplot2::aes(x=x, y=y))+
      ggplot2::geom_point(x=vals$iter, y=current_temperature(vals$iter, input$s_curve_amplitude, input$s_curve_center, input$s_curve_width), color="red")+
      # ggplot2::geom_segment(x=input$total_iterations, xend=input$total_iterations, y=0, yend=input$s_curve_amplitude/2, 
      #                       size=0.3,  linetype="dashed", color="red",
      #                       arrow = grid::arrow(angle=90, ends="both", length = grid::unit(0.1, "inches")) )+
      # ggplot2::geom_label(x=input$total_iterations, y=input$s_curve_amplitude/2, label="Amplitude/2", angle=90, color="red")+
      # ggplot2::geom_vline(xintercept = input$s_curve_center, linetype="dashed", color="blue")+
      # ggplot2::geom_label(x=input$s_curve_center, y=0, label="Center", angle=90, color="blue")+
      ggplot2::xlab("Iteration")+
      ggplot2::ylab("Temperature")+
      ggplot2::ggtitle("Annealing Schedule")
    
    plot(x,y, type='l', xlab="Iteration", ylab="Temperature", main = "Annealing Schedule")
    points(vals$iter, current_temperature(vals$iter, input$s_curve_amplitude, input$s_curve_center, input$s_curve_width), pch=21, col="red", bg='red')
  })
  output$route <- shiny::renderPlot({
    if (all(is.na(vals$distances))) return (edges.plot(points, edges_route))
    plot.tour(vals$best_tour, points, edges_route)
  })
  output$routemap <- leaflet::renderLeaflet({
    if (all(is.na(vals$distances))) return (edges.plot.map(points, edges_route))
    if (shiny::isolate(vals$iter) == shiny::isolate(vals$total_iterations)){
      plot.tour.map(vals$best_tour, points, edges_route, F)
    }else{
      plot.tour.map(vals$best_tour, points, edges_route, T)
    }
  })
  output$distance <- shiny::renderPlot({
    if (all(is.na(vals$distances))) return(plot.new())
    plot(vals$plot_every_iterations * (1:vals$number_of_loops), vals$distances,
         type='o', pch=19, cex=0.7, main="Evolution of Current Tour Distance",
         ylim=c(0, max(vals$distances, na.rm=TRUE)), xlab="iterations", ylab="current tour distance",
        )
    
  })
  session$onSessionEnded(function() {
    run_annealing_process$suspend()
  })
  
}

shiny::shinyApp(ui, server)
