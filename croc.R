# Artificial Intelligence - Project 2 - Where's Croc
# Author: Gunnar Atli Eggertsson  
# Date: 2018-09-25

# Usage:   generateProbs (readings, positions, old_probs, probs, nbs)
# Input:   i) Readings from croc's sensor
#         ii) Positions of tourists and ranger
#        iii) Previous probabilities of finding croc in different waterholes
#         iv) Means and stds of readings
#          v) List of all neighbors
# Output:  Probabilities of finding croc in each of the holes
generateProbs <- function (readings, positions, old_probs, probs, nbs) {
  
  nr_holes <- nrow(probs$salinity)
  
  # Check if first round
  if (sum(old_probs) == 0) {
    count <- 0
    holes <- vector(mode="double", length=nr_holes)
    for (i in 1:nr_holes) {
      # If a tourist is found at a given waterhole, we know that Croc is not there
      if ((!is.na(positions[1]) && !is.na(positions[2])) && (positions[1] == i || positions[2] == i || positions[3] == i)) {
          old_probs[i] <- 0
      }
      else {
        old_probs[i] <- 1
        count <- count + 1 
      }
    }
    # Probability of finding croc in each waterhole is 1 divided by the number of holes
    old_probs <- old_probs / count
  }
  
  # Initialize a vector for the probabilities of finding croc in each of the holes
  waterhole_prob <- vector(mode = "double", length = nr_holes)
  # Tourist eaten
  if ((positions[[1]]  < 0) && !is.na(positions[[1]])) {
    # If the first tourist was eaten we know that croc was there
    croc_loc <- -positions[[1]]
    waterhole_prob[croc_loc] <- 1
  }
  else if ((positions[[2]] < 0) && !is.na(positions[[2]])) {
    # If the second tourist was eaten we know that croc was there
    croc_loc <- -positions[[2]]
    waterhole_prob[croc_loc] <- 1
  }
  # Tourist not eaten
  else {
    # Compute the probabilities of croc being found in each of the holes, using the HMM forward-algorithm
    for (i in 1:nr_holes) {
      # Find the number of neighbors of each hole
      nr_nbs <-length(nbs[[i]])
      # Compute the emission, i.e. the probabilities of observing the different readings given the state of the system
      emission <- dnorm(readings[1], probs$salinity[i, 1], probs$salinity[i, 2]) *
                  dnorm(readings[2], probs$phosphate[i, 1], probs$phosphate[i, 2]) *
                  dnorm(readings[3], probs$nitrogen[i, 1], probs$nitrogen[i, 2])
      if ((!is.na(positions[1]) && i == positions[1]) || (!is.na(positions[2]) && i == positions[2])) {
        emission <- 0
      }
      # Probability of croc reaching the waterhole
      transmission <- 0
      for (j in 1:nr_nbs) {
        nb <- nbs[[i]][j]
        transmission <- transmission + (1.0 / length(nbs[[nb]])) * old_probs[nb]
      }
     waterhole_prob[i] <- emission * transmission 
    }
  }
  # Normalize
  waterhole_prob <- waterhole_prob / sum(waterhole_prob)
  return(waterhole_prob)
}

# Usage:  generateMove(new_probs, positions, edges)
# Input:  i) Probabilities for croc's location
#        ii) Positions of tourists and ranger
#       iii) Neighbors of each waterhole
generateMove <- function (new_probs, positions, nbs) {
  
  ranger <- positions[3]
  max_prob <- max(new_probs)
  croc_est <- which.max(new_probs)
  
  shortest_path <- find_shortest_path(nbs, ranger, croc_est)
  # At this point we have our shortest path in the variable shortest_path
  # Probability threshold
  thres <- 0.1
  next_move <- vector(mode="integer", length=2)
  if (length(shortest_path) == 1) {
    next_move[1] <- 0
    croc_est <- rev(order(new_probs))[2]
    shortest_path <- find_shortest_path(nbs, ranger, croc_est)
    next_move[2] <- shortest_path[2]
  }
  else if (length(shortest_path) == 2) {
    next_move[1] <- shortest_path[2]
    next_move[2] <- 0
  }
  else {
    next_move[1] <- shortest_path[2]
    if (new_probs[shortest_path[2]] > thres) {
      next_move[2] <- 0
    } else {
      next_move[2] <- shortest_path[3] 
    }
  }
  return(next_move)
}

# BFS (Adjusted from the following source: https://stackoverflow.com/questions/8922060/how-to-trace-the-path-in-a-breadth-first-search)
# Usage:  find_shortest_path(nbs, ranger, croc_est)
# Input:  i) Neighbors of each waterhole
#        ii) Position of the ranger (Initial node for our BFS search)
#       iii) The most probable location of Croc (Destination node for our BFS search)
# Output: A vector containing the nodes that form the shortest path between ranger and croc_est
find_shortest_path <- function(nbs, ranger, croc_est) {
  
  # We need to generate the shortest path between ranger and croc
  parent <- NULL
  queue <- list()
  queue[1] <- ranger
  visited <- vector(mode="integer")
  
  while (length(queue) > 0) {
    node <- queue[1]
    queue[1] <- NULL
    if (node == croc_est) {
      shortest_path <- as.numeric(backtrack(parent, ranger, croc_est))
      break
    }
    nbs_node <- nbs[[as.numeric(node)]][2:length(nbs[[as.numeric(node)]])]
    visited <- c(visited, as.numeric(node))
    for (i in nbs_node) {
      if (!(i %in% visited) && !(i %in% queue)) {
        parent[i] <- node
        queue[length(queue)+1] <- i
      }
    }
  }
  return(shortest_path)
}

# BFS-Backtracking (Adjusted from the following source: https://stackoverflow.com/questions/8922060/how-to-trace-the-path-in-a-breadth-first-search)
# Usage:  backtrack(parent, ranger, croc_est)
# Input:  i) Parent node
#        ii) Ranger position
#       iii) Croc most probable location
# Output: Shortest path from ranger to most probable location of croc
backtrack <- function(parent, ranger, croc_est) {
  path <- list(croc_est)
  while (tail(path, n=1) != ranger) {
    path[length(path) + 1] <- parent[as.numeric(tail(path, n=1))]
  }
  path <- rev(path)
  return(path)
}

# Usage:  myFunction (moveInfo, readings, positions, edges, probs)
# Input:  See documentation for runWheresCroc function
# Output: Information about the movement of the ranger
myFunction <- function(moveInfo,readings,positions,edges,probs) {
  
  nr_holes <- nrow(probs$salinity)
  nr_edges <- nrow(edges)
  
  ####################################################################################
  ################ Find all neighbors and put in a list of lists #####################
  ####################################################################################
  nbs <- list()
  # Add each node as its own neighbor
  for (i in 1:nr_holes) {
    nbs[i] <- list(i)
  }
  for (i in 1:nr_edges) {
    src <- edges[i, 1]
    dst <- edges[i, 2]
    # Number of currently found neighbors of the src node
    src_nbs <- length(nbs[[src]])
    # Number of currently found neighbors of the dst node
    dst_nbs <- length(nbs[[dst]])
    # Add the dst node to the neighbors of the src node
    nbs[[src]][src_nbs + 1] <- dst
    # Add the src node to the neighbors of the dst node
    nbs[[dst]][dst_nbs + 1] <- src
   }
  
  if (length(moveInfo$mem) == 0 || moveInfo$mem$status == 1) {
    moveInfo$mem[["old_probs"]] <- vector(mode = "double", length = nr_holes)
    # Handle the case when the test function is running game nr. n with n > 1
    moveInfo$mem$status <- 0
  }
  old_probs <- moveInfo$mem[["old_probs"]]
  
  # Calculate the probability of croc being found, in each of the waterholes
  new_probs <- generateProbs(readings, positions, old_probs, probs, nbs)
  
  # Find best moves
  moveInfo$moves <- generateMove(new_probs, positions, nbs)
  
  # Save the waterhole probabilities in the mem-field
  moveInfo$mem[["old_probs"]] <- new_probs
  return(moveInfo)
}

environment(myFunction) <- asNamespace('WheresCroc')