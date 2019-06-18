
# TSP Package -------------------------------------------------------------

if(F){
  # Nearest, farthest, cheapest and arbitrary insertion algorithms
  atsp <- TSP::ATSP(distance_matrix)
  tour <- TSP::solve_TSP(x = atsp, method="arbitrary_insertion")
  tour <- TSP::solve_TSP(x = atsp, method="nearest_insertion")
  tour <- TSP::solve_TSP(x = atsp, method="farthest_insertion")
  as.integer(tour)
  tour
}

# Simulated Annealing -----------------------------------------------------

calculate_tour_distance <- function(tour, distance_matrix) {
  sum(distance_matrix[embed(c(tour, tour[1]), 2)])
}

current_temperature <- function(iter, s_curve_amplitude, s_curve_center, s_curve_width) {
  s_curve_amplitude * s_curve(iter, s_curve_center, s_curve_width)
}

s_curve <- function(x, center, width) {
  1 / (1 + exp((x - center) / width))
}

run_intermediate_annealing_process <- function(points, distance_matrix, tour, tour_distance, best_tour, best_distance,
                                               starting_iteration, number_of_iterations,
                                               s_curve_amplitude, s_curve_center, s_curve_width) {
  n_points = nrow(points)
  for(i in 1:number_of_iterations) {
    iter = starting_iteration + i
    temp = current_temperature(iter, s_curve_amplitude, s_curve_center, s_curve_width)
    
    candidate_tour = tour
    swap = sample(n_points, 2)
    candidate_tour[swap[1]:swap[2]] = rev(candidate_tour[swap[1]:swap[2]])
    candidate_dist = calculate_tour_distance(candidate_tour, distance_matrix)
    
    if (temp > 0) {
      ratio = exp((tour_distance - candidate_dist) / temp)
    } else {
      ratio = as.numeric(candidate_dist < tour_distance)
    }
    
    if (runif(1) < ratio) {
      tour = candidate_tour
      tour_distance = candidate_dist
      
      if (tour_distance < best_distance) {
        best_tour = tour
        best_distance = tour_distance
      }
    }
  }
  return(list(tour=tour, tour_distance=tour_distance, best_tour=best_tour, best_distance=best_distance))
}

simulated.annealing <- function(points, distance_matrix){
  n <- nrow(points)
  tour <- sample(n)
  tour_distance <- calculate_tour_distance(tour, distance_matrix)
  best_tour <- c()
  best_distance <- Inf
  total_iterations <- 25000
  iter <- 0
  plot_every_iterations <- 500
  s_curve_amplitude <- 4000
  s_curve_center <- 0
  s_curve_width <- 3000
  
  number_of_loops <- ceiling(total_iterations / plot_every_iterations)
  distances <- rep(NA, number_of_loops)
  tours <- rep(NA, number_of_loops)
  
  for(i in 1:number_of_loops){
    intermediate_results <- run_intermediate_annealing_process(points, distance_matrix, tour, tour_distance, best_tour, best_distance,
                                                               iter, plot_every_iterations,
                                                               s_curve_amplitude, s_curve_center, s_curve_width)
    
    tour <- intermediate_results$tour
    tour_distance <- intermediate_results$tour_distance
    best_tour <- intermediate_results$best_tour
    best_distance <- intermediate_results$best_distance
    
    iter <- iter + plot_every_iterations
    
    distances[ceiling(iter / plot_every_iterations)] <- intermediate_results$tour_distance
    tours[[ceiling(iter / plot_every_iterations)]] <- list(intermediate_results$tour)
  }
  return(list(d=distances, tours=tours, best_tour=best_tour))
}
tour.result <- function(tour, edges_summary){
  embed(c(i=tour, tour[1]), 2) %>% 
    as.data.frame() %>% 
    plyr::rename(c("V1"="j","V2"="i")) %>% 
    merge(edges_summary) %>% 
    dplyr::summarise(total_time = sum(traffic_time),
                     total_distance = sum(distance))
}

# Linear Programming Problem ----------------------------------------------------------------

lp <- function(routes, points){
  
  nvar <- nrow(routes)
  nconstraint <- length(points)
  
  objective.in <- routes$traffic_time
  const.rhs <- rep(1,nconstraint)
  const.dir  <- rep("==", nconstraint)
  # const.mat <- 
  
  # Find the optimal solution
  optimum <-  lpSolve::lp(direction="min",
                          objective.in = objective.in,
                          const.mat = const.mat,
                          const.dir = const.dir,
                          const.rhs = const.rhs,
                          all.int = T)
  
  # Print status: 0 = success, 2 = no feasible solution
  print(optimum$status)
  # Display the optimum values for x_4p, x_3p and x_w
  best_sol <- optimum$solution
  names(best_sol) <- c("x_4p", "x_3p", "x_w") 
  print(best_sol)
  
  # Check the value of objective function at optimal point
  print(paste("Total cost: ", optimum$objval, sep=""))
}

# New LP TSP Function ------------------------------------------------------

tspsolve <- function(x){
  diag(x)<-1e10
  ## define some basic constants
  nx<-nrow(x)
  lx<-length(x)
  objective<-matrix(x,lx,nx)
  rowNum<-rep(row(x),nx)
  colNum<-rep(col(x),nx)
  stepNum<-rep(1:nx,each=lx)
  
  ## these constraints ensure that at most one edge is traversed each step
  onePerStep.con<-do.call(cbind,lapply(1:nx,function(i) 1*(stepNum==i)))
  onePerRow.rhs<-rep(1,nx)
  
  ## these constraints ensure that each vertex is visited exactly once
  onceEach.con<-do.call(cbind,lapply(1:nx,function(i) 1*(rowNum==i)))
  onceEach.rhs<-rep(1,nx)
  
  ## these constraints ensure that the start point of the i'th edge
  ## is equal to the endpoint of the (i-1)'st edge
  edge.con<-c()
  for(s in 1:nx){
    s1<-(s %% nx)+1    
    stepMask<-(stepNum == s)*1
    nextStepMask<- -(stepNum== s1)
    for(i in 1:nx){        
      edge.con<-cbind(edge.con,stepMask * (colNum==i) + nextStepMask*(rowNum==i))
    }
  }
  edge.rhs<-rep(0,ncol(edge.con))
  
  ## now bind all the constraints together, along with right-hand sides, and signs
  constraints<-cbind(onePerStep.con,onceEach.con,edge.con)
  rhs<-c(onePerRow.rhs,onceEach.rhs,edge.rhs)
  signs<-rep("==",length(rhs))
  list(constraints,rhs)
  
  ## call the lp solver
  res <- lpSolve::lp("min",objective,constraints,signs,rhs,transpose=F,all.bin=T)
  
  ## print the output of lp
  print(res)
  
  ## return the results as a sequence of vertices, and the score = total cycle length
  list(cycle=colNum[res$solution==1],score=res$objval)
}

# Dijkstra Algorithm for Shortest Path Tree ------------------------------------

if(F){
  
spt <- optrees::getShortestPathTree(nodes = 1:10, 
                                    arcs = routes %>% dplyr::select(i,j,traffic_time) %>% as.matrix(),
                                    algorithm = "Dijkstra", directed=F, check.graph = T)
spt$tree.arcs
plot.tour(spt$tree.nodes, routes_coords)

mst <- optrees::getMinimumSpanningTree(nodes = 1:10, 
                                       arcs = routes %>% dplyr::select(i,j,traffic_time) %>% as.matrix(),
                                       algorithm = "Prim", check.graph = T)
mst$tree.arcs
plot.tour(mst$tree.nodes, routes_coords)

mct <- optrees::getMinimumCutTree(nodes = 1:10, 
                                  arcs = routes %>% dplyr::select(i,j,traffic_time) %>% as.matrix(),
                                  check.graph = T)
mct$tree.arcs
plot.tour(mct$tree.nodes, routes_coords)


mab <- optrees::getMinimumArborescence(nodes = 1:10,
                                       arcs = routes %>% dplyr::select(i,j,traffic_time) %>% as.matrix(),
                                       check.graph = T)
mab$tree.arcs
plot.tour(mab$tree.nodes, routes_coords)

}

