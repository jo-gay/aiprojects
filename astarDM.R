# Artificial Intelligence - Project 1 - Delivery Man
# Author: Gunnar Atli Eggertsson
# 2018-09-11

astarDM <- function (roads, car, packages) 
{
  
  nextMove <- 0                                                                      # Which direction to go
  toGo <- 0                                                                          # Next place to visit
  loc <- c(car$x, car$y)                                                             # Car's current location
  
  nrOfPacks <- nrow(packages)
  perms <- permutations(nrOfPacks)
  tot_manhs <- vector(mode="integer", length=nrow(perms))
  
  # Determine total Manhattan distance for all possible orderings of pick-ups + deliveries
  for (i in 1:nrow(perms)) {
    tot_manhs[i] <- abs(packages[perms[i, 1], 1] - 1) + abs(packages[perms[i, 1], 2] - 1) +
                    abs(packages[perms[i, 1], 3] - packages[perms[i, 1], 1]) + abs(packages[perms[i, 1], 4] - packages[perms[i, 1], 2]) + 
                    abs(packages[perms[i, 2], 1] - packages[perms[i, 1], 3]) + abs(packages[perms[i, 2], 2] - packages[perms[i, 1], 4]) +
                    abs(packages[perms[i, 2], 3] - packages[perms[i, 2], 1]) + abs(packages[perms[i, 2], 4] - packages[perms[i, 2], 2]) +
                    abs(packages[perms[i, 3], 1] - packages[perms[i, 2], 3]) + abs(packages[perms[i, 3], 2] - packages[perms[i, 2], 4]) +
                    abs(packages[perms[i, 3], 3] - packages[perms[i, 3], 1]) + abs(packages[perms[i, 3], 4] - packages[perms[i, 3], 2]) +
                    abs(packages[perms[i, 4], 1] - packages[perms[i, 3], 3]) + abs(packages[perms[i, 4], 2] - packages[perms[i, 3], 4]) +
                    abs(packages[perms[i, 4], 3] - packages[perms[i, 4], 1]) + abs(packages[perms[i, 4], 4] - packages[perms[i, 4], 2]) +
                    abs(packages[perms[i, 5], 1] - packages[perms[i, 4], 3]) + abs(packages[perms[i, 5], 2] - packages[perms[i, 4], 4]) +
                    abs(packages[perms[i, 5], 3] - packages[perms[i, 5], 1]) + abs(packages[perms[i, 5], 4] - packages[perms[i, 5], 2])
  }
  
  # Find the ordering that has the lowest Manhattan distance
  min_route <- perms[which.min(tot_manhs), ]
  
  if (car$load == 0) {
    
    packs_left <- which(packages[, 5] == 0)                                                 # Check which packages are left
    dest <- packages[intersect(min_route, packs_left)[1], 1:2]                              # Which package to go for next
  }
  
  # If a package is being transported we head to the the delivery point
  else {
    ind = which(packages[, 5] == 1)
    dest = packages[ind, 3:4]                                                                                                                                   
  }
 
   ################# Heuristic ########################
  # Matrix dimensions
  x_dim = dim(roads$hroads)[2]
  y_dim = dim(roads$vroads)[1]
  h = matrix(nrow = x_dim, ncol = y_dim)
  
  for (i in 1:x_dim) {
    for (j in 1:y_dim) {
      h[i,j] = abs(dest[1] - i) + abs(dest[2] - j)
    }
  }
  
  ################ Frontier ###########################
  expanded = c(car$x, car$y)
  cost_to_expanded = 0
  
  # Dealing with the path
  path <- matrix(ncol = 2)
  path[1,] <- c(expanded[1], expanded[2])
  already_visited <- matrix(ncol = 2)
  already_visited[1,] <- c(expanded[1], expanded[2])
  counter = 1
  while (expanded[1] != dest[1] || expanded[2] != dest[2]) {
    
    to_be_deleted = c()
    new_frontier = list()
    
    if (counter > 1) {
      path <- rbind(path, c(expanded[1], expanded[2]))
      already_visited <- rbind(already_visited, c(expanded[1], expanded[2]))
    }
    
    
    x <- expanded[1]  # X-coordinate of expanded node
    y <- expanded[2]  # Y-coordinate of expanded node
    ################ Find all neighbors of expanded node ##################
    n = matrix(nrow = 4, ncol = 2)
    n[, 1] = c(x-1, x, x, x+1)
    n[, 2] = c(y, y-1, y+1, y)
    # Remove neighbors outside grid
    n = n[n[,1] > 0,]
    n = n[n[,2] > 0,]
    n = n[n[,1] < x_dim+1,]
    n = n[n[,2] < y_dim+1,]
    
    
    
    # Add the neighbors to the frontier
    count = 0
    for (i in 1:nrow(n)) {
      
      addNeighbor = TRUE
      
      for (j in 1:nrow(already_visited)) {
        if (n[i, 1] == already_visited[j, 1] && n[i, 2] == already_visited[j, 2]) {
          addNeighbor = FALSE
        }
      }
      
      if (addNeighbor) {
        
        count = count + 1  
        if (n[i, 1] < x) {                                                                                         # Moving to the left
          cost = cost_to_expanded + roads$hroads[n[i, 1], n[i, 2]] + h[n[i, 1], n[i, 2]]                                                                                          
          node = list(c(n[i, 1], n[i, 2]), as.numeric(cost), cost_to_expanded + roads$hroads[n[i, 1], n[i, 2]] , path)
        }
        else if (n[i, 1] > x) {                                                                                    # Moving to the right
          cost = cost_to_expanded + roads$hroads[n[i, 1]-1, n[i, 2]] + h[n[i, 1], n[i, 2]]
          node = list(c(n[i, 1], n[i, 2]), as.numeric(cost), cost_to_expanded + roads$hroads[n[i, 1]-1, n[i, 2]], path)                                
        }
        else if (n[i, 1] == x && n[i,2] < y) {                                                                     # Moving down
          cost = cost_to_expanded + roads$vroads[n[i, 1], n[i, 2]] + h[n[i, 1], n[i, 2]]
          node = list(c(n[i, 1], n[i, 2]), as.numeric(cost), cost_to_expanded + roads$vroads[n[i, 1], n[i, 2]], path)        
        }
        else {                                                                                                     # Moving up
          cost = cost_to_expanded + roads$vroads[n[i, 1], n[i, 2]-1] + h[n[i, 1], n[i, 2]]
          node = list(c(n[i, 1], n[i, 2]), as.numeric(cost), cost_to_expanded + roads$vroads[n[i, 1], n[i, 2]-1], path)       
        }
        new_frontier[[count]] <- node
      }
    }
    if (exists("old_frontier")) {
      new_frontier = c(new_frontier, old_frontier)
    }
    
    # Order the frontier by the cost plus heuristic
    new_frontier = new_frontier[order(sapply(new_frontier, `[[`, i=2))]
    
    # If the estimated costs of the first nodes are equal, go for the one that's closest (Manhattan) to the destination
    if (length(new_frontier) > 1 && new_frontier[[1]][[2]] == new_frontier[[2]][[2]]) {
      manhs = c()
      manhs[1] = abs(dest[1] - new_frontier[[1]][[1]][[1]]) + abs(dest[2] - new_frontier[[1]][[1]][[2]])
      manhs[2] = abs(dest[1] - new_frontier[[2]][[1]][[1]]) + abs(dest[2] - new_frontier[[2]][[1]][[2]])
      
      if (length(new_frontier) > 2) {
        for (i in 3:length(new_frontier)) {
          if (new_frontier[[1]][[2]] == new_frontier[[i]][[2]]) {
            manh <- abs(dest[1] - new_frontier[[i]][[1]][[1]]) + abs(dest[2] - new_frontier[[i]][[1]][[2]])
            manhs <- manhs[i] <- manh
          } else {
            break
          }
        }
      }
      
      lowest_manh <- which.min(manhs)
      
      # Node to expand
      expanded = new_frontier[[lowest_manh]][[1]]
      # Path to append
      path = new_frontier[[lowest_manh]][[4]]
      cost_to_expanded = new_frontier[[lowest_manh]][[3]]
      tot_cost = new_frontier[[lowest_manh]][[2]]
      # Delete the expanded node from the frontier
      new_frontier[lowest_manh] <- NULL
    } 
    else if (length(new_frontier) > 0){
      # Go to the node with the lowest cost
      expanded = new_frontier[[1]][[1]]
      path = new_frontier[[1]][[4]]
      cost_to_expanded = new_frontier[[1]][[3]]
      new_frontier[1] <- NULL
    }
    
    # Remove duplicate nodes from frontier
    if (length(new_frontier) > 1) {
      for (i in 1:(length(new_frontier)-1)) {
        for (j in (i+1):length(new_frontier)) {
          if (new_frontier[[i]][[1]][1] == new_frontier[[j]][[1]][1] && new_frontier[[i]][[1]][2] == new_frontier[[j]][[1]][2]) {
            to_be_deleted = c(to_be_deleted, j)
          } 
        }
      } 
    }
    if (length(to_be_deleted) > 0) {
      to_be_deleted = unique(to_be_deleted)
      new_frontier <- new_frontier[-to_be_deleted]
    }
    
    # Check if we have reached our destination
    if(expanded[1] == dest[1] && expanded[2] == dest[2]) {  
      final_path = rbind(path, c(expanded[1], expanded[2]))
    } else {
      old_frontier <- new_frontier
    }
    counter = counter + 1
  }
  
  if (loc[1] == dest[1] && loc[2] == dest[2]) {
    nextMove = 5
  } else {
    toGo = c(final_path[2,1], final_path[2,2])
    if (loc[1] < toGo[1]) {
      nextMove = 6
    }
    else if (loc[1] > toGo[1]) {
      nextMove = 4
    }
    else if (loc[2] < toGo[2]) {
      nextMove = 8
    }
    else {
      nextMove = 2
    }
   
  }
  
  car$nextMove = nextMove
  car$mem = list()
  return(car)
}


# Find all permutations of a list
# Source: https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
permutations <- function(packs){
  if(packs==1){
    return(matrix(1))
  } else {
    recur <- permutations(packs-1)
    p <- nrow(recur)
    mat <- matrix(nrow=packs*p,ncol=packs)
    for(i in 1:packs){
      mat[(i-1)*p+1:p,] <- cbind(i,recur+(recur>=i))
    }
    return(mat)
  }
}


environment(astarDM) <- asNamespace('DeliveryMan')
