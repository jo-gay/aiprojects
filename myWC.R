#moveInfo$mem$status is zero in the first game (unless you change it) and is 1 in subsequent games.
myFunction<-function(moveInfo, readings, positions, edges, probs)
{
  #Search non-optimal holes that we are passing if the probability of finding the croc there is greater than...
  searchThreshold<-0.1

  #Find out how many holes there are (always 40)
  size<-max(edges)
  
  #Get probs from the last step. Recalculate if it's not there or if moveInfo$moves is null 
  #which means we just started a new game
  if(is.null(moveInfo$mem$probs) || is.null(moveInfo$moves)) {
    prevStateProbs<-rep(1/(size-3), size)
    #Croc can't start in the same spot as ranger or tourists
    prevStateProbs[positions]<-0
  }
  else {
    prevStateProbs<-moveInfo$mem$probs
  }
  
  #Check if we have a death - if so, we know exactly where croc is so ignore emissions
  if(!is.na(positions[1]) && positions[1]<0) {
    emissionProbs<-rep(0, size)
    emissionProbs[-positions[1]]<-1
  }
  else if(!is.na(positions[2]) && positions[2]<0) {
    emissionProbs<-rep(0, size)
    emissionProbs[-positions[2]]<-1
  }
  else {
    #Find prob that croc is in each hole, based on readings and tourist positions
    emissionProbs<-holeDFs(probs, readings)
    if(!is.na(positions[1])) {
      emissionProbs[positions[1]]<-0
    }
    if(!is.na(positions[2])) {
      emissionProbs[positions[2]]<-0
    }
    emissionProbs<-emissionProbs/sum(emissionProbs)
  }
  
  if(is.null(moveInfo$mem$tMatrix)){
    moveInfo$mem$tMatrix<-createTransitonMatrix(edges)
  }
  #Multiply previous state probabilities by the transition matrix to get a prob for each state
  stateProbs<-prevStateProbs%*%moveInfo$mem$tMatrix
  #Then multiply by the emission prob for that state and normalize
  posterior<-emissionProbs*stateProbs
  moveInfo$mem$probs<-posterior/sum(posterior)
  
  #print(c('most likely posterior:',which(posterior==max(posterior))))
  
  #Now we know the probability that croc is currently in each hole. Where should we go?
  #First find the most likely hole
  target<-which.max(moveInfo$mem$probs)
  #print(target)
  #Decide whether to search the current hole
  if(target==positions[3] || moveInfo$mem$probs[positions[3]]>searchThreshold) {
    firstMove<-0
    #if we search, set prior prob to zero for next turn
    moveInfo$mem$probs[positions[3]]<-0
  }
  else {
    firstMove<-bestFirstPathNext(positions[3], target, edges, moveInfo$mem$probs)
  }
  
  #If we are already at the most likely hole, hopefully we find croc, but if not need to move on.
  #Pick the next most likely hole for the new target
  if(target==positions[3]) {
    target<-which.max(moveInfo$mem$probs[-target])
  }

  #If the first move is not a search, decide whether the second move is a search.
  if(firstMove > 0) {
    if(moveInfo$mem$probs[firstMove]>searchThreshold || firstMove == target) {
      secondMove<-0
      moveInfo$mem$probs[firstMove]<-0
    }
    else {
      secondMove<-bestFirstPathNext(firstMove, target, edges, moveInfo$mem$probs)
    }
  }
  else {
    secondMove<-bestFirstPathNext(positions[3], target, edges, moveInfo$mem$probs)
    #secondMove<-0
  }
  moveInfo$moves<-c(firstMove,secondMove)
  return(moveInfo)
}

#Calculate the sum of the pdfs for each hole for a given reading
holeDFs<-function(probs, readings)
{
  return(dnorm(readings[1], probs$salinity[,1], probs$salinity[,2])*
           dnorm(readings[2], probs$phosphate[,1], probs$phosphate[,2])*
           dnorm(readings[3], probs$nitrogen[,1], probs$nitrogen[,2]))
}

#Create the transition matrix from each hole to each other hole
createTransitonMatrix<-function(edges)
{
  size<-max(edges)
  tMatrix<-matrix(c(0), nrow=size, ncol=size)
  for(i in 1:size) {
    conns<-c(edges[edges[,1]==i,2], edges[edges[,2]==i,1])
    tMatrix[i,conns]<-1/(length(conns)+1)
    tMatrix[i,i]<-1/(length(conns)+1)
  }
  return(tMatrix)
}

#Find the shortest route to the target hole using best first - only return the next hole, 
#the whole path is not required
#If probs are supplied, reduce the travel cost for any edge leading to hole i by probs[i] if probs[i]
#is above a threshold, in order to encourage routes that visit high-probability holes.
bestFirstPathNext<-function(from, to, edges, probs=NULL)
{
  if(is.null(probs)){
    probs<-rep(0, max(edges))
  }
  threshold<-0.01
  
  done<-c()
  stop<-FALSE
  #Frontier contains the hole number, num moves to get there, and first hole to visit
  frontier<-matrix(c(from,0,0), nrow=1)
  while(!stop){
    exploreRow<-which(frontier[,1]==from)
    cost<-frontier[exploreRow, 2]
    firstStep<-frontier[exploreRow, 3]
    neighbours<-c(edges[edges[,1]==from,2], edges[edges[,2]==from,1])
    for(i in neighbours) {
      if(probs[i]>threshold) {
        #High-prob hole that we would like to visit if it's en route. Reduce cost for this edge
        w<-1-probs[i]
      }
      else {
        w<-1
      }
      if(firstStep==0) {
        nextHole<-i
      }
      else {
        nextHole<-firstStep
      }
      
      if(i==to) {
        stop<-TRUE
        break
      }
      
      if(!is.element(i, done)){
        if(!is.element(i, frontier[,1])){
          frontier<-rbind(frontier, c(i, cost+w, nextHole))
        }
        else {
          frow<-which(frontier[,1]==i)
          if(frontier[frow,2]>cost+w) {
            frontier[frow,2]<-cost+w
            frontier[frow,3]<-nextHole
          }
        }
      }
    }
    done<-c(done,from)
    frontier<-frontier[-exploreRow,]
    if(is.null(dim(frontier))) {
      from<-frontier[1]
    }
    else {
      from<-frontier[which.min(frontier[,2])[1],1]
    }
  }
  return(nextHole)
}