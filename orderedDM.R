#Artificial Intelligence Autumn 2018
#Project 1: Delivery Man
#Group 131: Gunnar Eggertsson & Jo Gay

#Ordered AStar algorithm.
#Decide what order to pick up the packages to minimise the total travel distance
#ignoring traffic. Then use A* algorithm to find the route to the next pickup/dropoff
myFunction<-function(roads, car, packages)
{
  #The first time we are called, decide what order we are going to get
  #the packages in and store in car$mem (which has zero length to begin with). Also remember
  #whether loaded so that later, after we drop off a package (memory of load is >0 but 
  #current load is zero), we can recalculate the sequence in case we have picked up the 
  #wrong one by mistake
  remainingPs<-matrix(packages[which(packages[,5]==0),], ncol=5)
  
  gridSize<-dim(roads$hroads)+c(1,0)
  numNodes <- gridSize[1]*gridSize[2]
  
  if(length(car$mem)==0 || (car$mem$lastLoad>0 && car$load==0)) {
    numPackages<-dim(remainingPs)[1]
    distances<-matrix(c(0), nrow=numPackages+1, ncol=numPackages+1)
    #For each package, calculate the distance from the drop-off to each other package
    for(i in 1:numPackages){
      for(j in 1:numPackages) {
        if(i==j) {
          distances[[i,i]]<-Inf
        }
        else {
          distances[[j, i]]<-manhattanDist(list(x=remainingPs[j, 3], y=remainingPs[j, 4]), remainingPs[i, 1:2])
        }
      }
      #Calculate the distance from the start point to each package
      distances[[numPackages+1,i]]<-manhattanDist(car, remainingPs[i,1:2])
    }
    colnames(distances)<-c(1:numPackages,"End")
    rownames(distances)<-c(1:numPackages,"Start")
    #Select the combination that gives the shortest overall travel distance
    car$mem$seq<-orderPackages(distances, numPackages+1)
  }

  car$mem$lastLoad<-car$load
  
  frontier<-list()
  frontier$data<-matrix(c(0), nrow=1, ncol=6)
  colnames(frontier$data)<-c("index", "x", "y", "g", "h", "cost")
  #Each node in the frontier has an associated path. These are stored
  #in a list of length numNodes, the path for node i is the ith element in the list
  frontier$paths<-vector(mode="list", length=numNodes)
  frontier$done<-rep(0,numNodes)
  #Fill in the frontier details for the starting point, this will be the first entry in the frontier's data
  #and will be first to be expanded
  index<-car$x+(car$y-1)*gridSize[1]
  frontier$paths[[index]]<-c(index)
  
  if(car$load) {
    destination<-matrix(packages[which(packages[, 5] == 1),], ncol=5)[,3:4]
    h<-manhattanDist(car, destination)
    frontier$data[1,]<-c(index=index, x=car$x, y=car$y, g=0, 
                         h=h, cost=h)
    path<-exploreNodes(roads, destination, car, gridSize, frontier)
  }
  else {
    nextPackage<-car$mem$seq[1]
    destination<-remainingPs[nextPackage, 1:2]
    h<-manhattanDist(car, destination)
    frontier$data[1,]<-c(index=index, x=car$x, y=car$y, g=0, 
                         h=h, cost=h)
    path<-exploreNodes(roads, destination, car, gridSize, frontier)
    
    #Use this version instead to skip pre-ordering of packages and just take greedy route (nearest first)
    # destination<-remainingPs[, 1:2]
    # frontier$data[1,]<-c(index=index, x=car$x, y=car$y, g=0, h=min(manhattanDist(car, destination)), status=1)
    # path<-exploreNodes(roads, destination, car, gridSize, frontier)
  }
  car$nextMove<-5
  #if path only has one node in, it is the node that the car is on. This means that we have pickup and dropoff
  #on the same square.
  if(length(path)==1) {
    return(car)
  }
  #Shortest path to the nearest package (or destination) has been found. We only need to 
  #set the next move, so take the second node in the path (first node is this one)
  x<-((path[2]-1)%%gridSize[1])+1
  y<-floor((path[2]-1)/gridSize[1])+1
  
  if(car$x < x) {
    car$nextMove <- 6
  }
  else if(car$y < y) {
    car$nextMove <- 8
  }
  else if(car$x > x) {
    car$nextMove <- 4
  }
  else {
    car$nextMove <- 2
  }

  return(car)
}

#Given distance matrix (non-symmetric), choose the order of visits to minimise
#the total travel. One element from each row and column. Start gives the row of the
#last package to be visited.
#Weight earlier distances a small amount to encourage greedy actions, since traffic 
#conditions are not known. Weight 1.05 better than 1.1. Could refine further but the
#variance is quite high.
#Return a list containing the cost and the best sequence
orderPackages<-function(distances, start, weight=1.05)
{
  numPackages<-dim(distances)[1]-1
  if(numPackages==1) {
    return(c(1))
  }
  allPerms<-findPerms(numPackages)

  best<-list(cost=Inf, sequence=c())
  for(i in 1:dim(allPerms)[1]) {
    seq<-allPerms[i,]
    cost<-distances[start,seq[1]]
    for(j in 1:(numPackages-1)) {
      cost<-cost*weight+distances[seq[j], seq[j+1]]
    }
    if(cost < best$cost) {
      best$cost<-cost
      best$sequence<-seq
    }
  }
  return(best$sequence)
}

#Find all permutations of the integers from 1 to n inclusive and
#return them in an (n! x n) matrix
findPerms<-function(n) {
  if(n==1){
    return(matrix(1))
  }
  if(n==2){
    return(matrix(c(1,2,2,1), nrow=2))
  }
  oneLess<-findPerms(n-1)
  rows<-dim(oneLess)[1]
  result<-cbind(rep(n, rows), oneLess)
  for(i in 1:(n-2)){
    result<-rbind(result, cbind(oneLess[,1:i], rep(n, rows), oneLess[,(i+1):(n-1)]))
  }
  result<-rbind(result, cbind(oneLess, rep(n, rows)))

  return(result)
}


#Use index of a node to get the x and y coordinates
getCoords<-function(index, gridSize) 
{
  pos<-list(x=((index-1)%%gridSize[1])+1, y=floor((index-1)/gridSize[1])+1)
  return(pos)
}

#Take first node in frontier, add its neighbours to the frontier (or amend them if already 
#there with higher cost), remove the node itself from the frontier,
#and return the amended frontier, sorted with lowest cost unexplored node first
expandNode<-function(roads, destinations, gridSize, frontier)
{
  #Find the index of the node that we are expanding and check whether it is itself a goal node
  index<-frontier$data[[1, 'index']]
  path<-frontier$paths[[index]]
  goalNode<-frontier$data[[1, 'h']]==0
  if(goalNode) {
    return(path)
  }
  
  x<-((index-1)%%gridSize[1])+1
  y<-floor((index-1)/gridSize[1])+1
  
  #Check the 4 neighbours and if they are within the grid and not already in
  #the path taken to get here, remember the index.
  neighbours<-c()
  cost<-c()
  if(x>1) {
    if(!frontier$done[index-1]) {
      neighbours<-c(neighbours,index-1)
      cost<-c(cost, roads$hroads[x-1, y]+frontier$data[[1, 'g']])
    }
  }
  if(x<gridSize[1]) {
    if(!frontier$done[index+1]) {
      neighbours<-c(neighbours,index+1)
      cost<-c(cost, roads$hroads[x, y]+frontier$data[[1, 'g']])
    }
  }
  if(y>1) {
    if(!frontier$done[index-gridSize[1]]) {
      neighbours<-c(neighbours,index-gridSize[1])
      cost<-c(cost, roads$vroads[x, y-1]+frontier$data[[1, 'g']])
    }
  }
  if(y<gridSize[2]) {
    if(!frontier$done[index+gridSize[1]]) {
      neighbours<-c(neighbours,index+gridSize[1])
      cost<-c(cost, roads$vroads[x, y]+frontier$data[[1, 'g']])
    }
  }
  
  #We now have a list of nodes that are neighbours for the current one and are not
  #in the path we took to get here. Put these
  #in the frontier unless they are already there with a lower distance
  if(length(neighbours)>0){
    for(i in 1:length(neighbours)){
      #Is it already included in the data?
      if(is.element(neighbours[i], frontier$data[,'index'])) {
        frow<-which(frontier$data[,'index']==neighbours[i])
        #Have we just found a better route to it?
        if(frontier$data[[frow, 'g']]>cost[i]) {
          frontier$data[[frow, 'g']]<-cost[i]
          frontier$data[[frow, 'cost']]<-cost[i]+frontier$data[[frow, 'h']]
          frontier$paths[[neighbours[i]]]<-c(frontier$paths[[index]], neighbours[i])
        }
      }
      else {
        pos<-getCoords(neighbours[i], gridSize)
        h<-manhattanDist(pos, destinations)
        frontier$data<-rbind(frontier$data, c(index=neighbours[i], x=pos$x, y=pos$y, 
                                              g=cost[i], h=h, 
                                              cost=cost[i]+h))
        frontier$paths[[neighbours[i]]]<-c(frontier$paths[[index]], neighbours[i])
      }
    }
  }
  frontier$done[[index]]<-1
  #Take out the node we just explored
  frontier$data<-frontier$data[-1,]
  #Now the frontier is updated, reorder it and continue.
  if(length(frontier$data)>6) {
    #If more than one node in frontier, reorder by g+h
    # frontier$data<-frontier$data[order(sapply(frontier$data[,"cost"],head,1),decreasing=F),]
    #Reordering is very slow. Instead swap the best node with the first node.
    nextIdx<-which.min(frontier$data[,"cost"])
    if(nextIdx > 1) {
      nextNode<-frontier$data[nextIdx,]
      frontier$data[nextIdx,]<-frontier$data[1,]
      frontier$data[1,]<-nextNode
    }
  }
  else {
    #if only one node is left in the frontier, need to convert back to a matrix (very rarely happens)
    cn<-names(frontier$data)
    frontier$data<-matrix(frontier$data, ncol=6)
    colnames(frontier$data)<-cn
  }
  
  return(frontier)
}

#Iteratively explore the grid, taking the first node from the frontier, expanding it,
#checking whether we found a goal node, and then repeating.
exploreNodes<-function(roads, destinations, car, gridSize, frontier)
{
  stop<-FALSE
  while(!stop){
    if(frontier$data[[1,'h']]==0) {
      stop<-TRUE
    }
    frontier<-expandNode(roads, destinations, gridSize, frontier)
  }
  return(frontier)
}

#Calculate manhattan distance between current position and each package
#If loaded, use columns 3 and 4 in packages. Otherwise 1 and 2.
#Packages should be restricted to acceptable destinations (uncollected
#packages, or the one that is currently being delivered)
manhattanDist<-function(car, packages, loaded=FALSE)
{
  offset<-0
  if(loaded) {
    offset<-2
  }
  if(is.null(dim(packages))) {
    dist<-abs(packages[1+offset]-car$x)
    dist<-dist+abs(packages[2+offset]-car$y)
  }
  else {
    dist<-abs(packages[,1+offset]-car$x)
    dist<-dist+abs(packages[,2+offset]-car$y)
  }
  return(dist)
}

#Weight of 1.05 seems best with the default seed but 1.1 seems better with many other seeds.
#Test both with same set of several seeds and choose the best. Adjust weight parameter above 
#in between runs.
testSeeds<-function(seeds)
{
  results<-c()
  for(i in 1:length(seeds)) {
    results<-c(results, testDM(myFunction, seed=seeds[i], n=250))
  }
  results
}