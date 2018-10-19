myController<-function(maze) 
{
  decideAction = function(cont, maze_) {
    
    #This gets called first (before update) so check if we need to initialize the controller, and do so.
    doInitialSetup<-function(cont, themaze) {
      #Define a sub-function that turns a (state,action) pair into the resulting state (1-27).
      moveState <- function(state, action) {
        coords<-themaze$maze[state,]
        if(action == 2)
          newcoords<-c(coords[[1]], coords[[2]]-1)
        else if(action == 4)
          newcoords<-c(coords[[1]]-1, coords[[2]])
        else if(action == 6)
          newcoords<-c(coords[[1]]+1, coords[[2]])
        else if(action == 8)
          newcoords<-c(coords[[1]], coords[[2]]+1)
        else
          newcoords<-coords
        #Now we have the x and y coordinates (which may not be valid). Find the state index from the maze (1-27)
        if(newcoords[1]<1 || newcoords[1]>6 || newcoords[2]<1 || newcoords[2]>6){
          return(NA)
        }
        cont$stateLookup[newcoords[[1]], newcoords[[2]]]
      }
      
      #Set up state values (state = wobbie's position & monster's position)
      numStates<-length(themaze$maze[,1])
      cont$numStates<-numStates #store the number of states that Wobbie can be in (square this for total states)

      cont$epsilon<-0.5 #Initial probability of taking a random action
      cont$lambda<-0.75 #Initial learning rate
      cont$gamma<-1 #Discount rate
      cont$completed<-0 #number of games played so far
      
      #Set up utility values for each possible move from each state. One row for each combination of Wobbie and Monster's
      #positions, one column for each move.
      cont$utility<-matrix(rep(0, numStates*numStates*5), nrow=numStates*numStates)
      colnames(cont$utility)<-c(2,4,6,8,5)

      #Create a lookup matrix where x and y are the row and column indices and it gives the state number (1-27 and some NAs)
      cont$stateLookup<-matrix(rep(NA, 36), nrow=6)
      for(s in 1:numStates) {
        pos<-themaze$maze[s,]
        cont$stateLookup[pos[[1]], pos[[2]]]<-s
      }
      #Create a transition matrix giving the possible moves from each state, and where you end up
      cont$trans<-matrix(rep(NA, numStates*4), nrow=numStates)
      for(s in 1:numStates) {
        pos<-themaze$maze[s,]
        
        for(i in 1:4) {
          resultingState <- moveState(s, i*2)
          if(!is.na(resultingState)) {
            cont$trans[[s,i]] <- resultingState
          }
        }
      }
      
      #Set initial utility values to NA for impossible moves 
      #PLUS extra steps to try to improve accuracy (disabled as it didn't help): 
      # initialise utility to -1000 if Wobbie state = Monster state
      # and to 1000 for goal state
      # and to 1000 * gamma^(manhattan dist) - dist for others
      goal<-themaze$goal
      for(i in 1:numStates) {
        #get the x and y coordinates for the ith state
        pos<-themaze$maze[i,]
        
        # #DISABLED - initialisation of utility values based on knowledge of goal state and failure state
        # #Set the utility for both Wobbie and the monster being in this state to -1000 for all actions
        # cont$utility[i+numStates*(i-1),]=rep(-1000,5)
        # #Set the utility for Wobbie being in state 23 while the monster is in this state to 1000 for all actions
        # cont$utility[23+(i-1)*numStates,]=rep(1000,5)
        
        #For each of the 4 moves, check if it is possible. If not, set to NA.
        for(a in 1:4) {
          moveTo<-moveState(i, a*2)
          if(is.na(moveTo)) {
            cont$utility[i+numStates*(0:(numStates-1)),a]<-NA
          }
          # #DISABLED - initialisation of utility values based on distance from goal.
          # else {
          #   dist<-abs(themaze$maze[moveTo,1]-goal[1])-abs(themaze$maze[moveTo,2]-goal[2])
          #   cont$utility[i+numStates*(0:(numStates-1)),1]<-1000*(cont$gamma^dist)-dist
          # }
        }

      }
      
      
      return(cont)
    }
    if(is.null(cont$utility)) {
      cont<-doInitialSetup(cont, maze_)
    }

    #Now we have a utility matrix. Work out which state we are in (states are numbered as 1=(W1,M1), 2=(W2,M1), 3=(W3,M1)
    #27=(W27,M1), 28=(W1,M2) etc) where 1-27 correspond to the 27 spots available on the 6x6 grid
    numStates<-cont$numStates
    #Find the index of Wobbie and the monster's states
    wState=cont$stateLookup[maze_$wobbie[1], maze_$wobbie[2]]
    mState=cont$stateLookup[maze_$monster1[1], maze_$monster1[2]]
    
    #Record what state we are in for next time
    cont$fromState<-wState+((mState-1)*numStates)

    #Get the current values of the utility function for this state and check which of the 4 moves are possible
    utils<-as.vector(cont$utility[cont$fromState,])
    possMoves<-which(!is.na(utils))*2

    #Draw a random number and if less than epsilon, make a random move (unless controller's doRand flag has been set
    #to false, then always do greedy action)
    if(cont$doRand && runif(1) < cont$epsilon) {
      move = sample(possMoves, 1)
    }
    else {
      #Take the greedy action which has the max utility function
      move=which.max(utils)*2
    }
    if(move==10) move<-5
    list(move = move, control = cont)
  }
  
  #Function to update the controller's utility values on the basis of current state and last move
  update <- function(cont, themaze) {
    
    #Define a sub-function that turns a (state,action) pair into the resulting state (1-27).
    moveState <- function(state, action) {
      coords<-themaze$maze[state,]
      if(action == 2)
        newcoords<-c(coords[[1]], coords[[2]]-1)
      else if(action == 4)
        newcoords<-c(coords[[1]]-1, coords[[2]])
      else if(action == 6)
        newcoords<-c(coords[[1]]+1, coords[[2]])
      else if(action == 8)
        newcoords<-c(coords[[1]], coords[[2]]+1)
      else
        newcoords<-coords
      #Now we have the x and y coordinates (which may not be valid). Find the state index from the maze (1-27)
      if(newcoords[1]<1 || newcoords[1]>6 || newcoords[2]<1 || newcoords[2]>6){
        return(NA)
      }
      cont$stateLookup[newcoords[[1]], newcoords[[2]]]
    }
    
    if(themaze$finished || !themaze$alive) {
#      cont$completed = cont$completed+1
      # cont$epsilon = cont$epsilon - 0.000095 #reduce epsilon uniformly from 1 to 0.05 over 10,000 games
#      cont$epsilon = 1-(cont$completed*0.0001) #reduce epsilon uniformly from 1 to 0 over 10,000 games
#      cont$epsilon = 0.5-(cont$completed*0.00005) #reduce epsilon uniformly from 0.5 to 0 over 10,000 games
      cont$epsilon = cont$epsilon-0.00005 #reduce epsilon uniformly from starting level by 0.5 over 10,000 games
#       
#       #reduce lambda in steps (assumes starting value is 0.1, set above)
#       if(cont$completed == 2500) {
#        cont$lambda=0.075
#       }
#       if(cont$completed == 7500) {
#         cont$lambda=0.05
#       }
    }

    #Work out which row in the utility function table corresponds to the current positions of Wobbie and Monster
    wState=cont$stateLookup[[themaze$wobbie[1], themaze$wobbie[2]]]
    mState=cont$stateLookup[[themaze$monster1[1], themaze$monster1[2]]]

    utilRow<-wState+(mState-1)*cont$numStates
    oldUtilRow<-cont$fromState
    if(themaze$lastAction==5)
      utilCol<-5
    else
      utilCol<-themaze$lastAction/2
    
    oldQ<-cont$utility[[oldUtilRow, utilCol]]
    newQ<-themaze$reward + cont$gamma*max(cont$utility[utilRow,],na.rm=TRUE)
    cont$utility[[oldUtilRow, utilCol]]<-oldQ-cont$lambda*(oldQ-newQ)

    return(cont)
    
    # #Find which positions the monster might end up in
    # mStates<-cont$trans[mState, !is.na(cont$trans[mState,])]
    # 
    # #For each action, if it's a valid action, find the new Q value
    # for(i in 1:5) {
    #   oldQ <- cont$utility[[utilRow,i]]
    #   if(!is.na(oldQ)) {
    #     # if(oldQ > 1000 || oldQ < -1000) {
    #     #   print("Q is v big/small")
    #     # }
    #     q <- 0
    #     wNewState <- moveState(wState, 2*i)
    #     if(wNewState == mState) {
    #       #If you move into the state that the monster is in, it kills you before it's had its move
    #       q<-max(cont$utility[wNewState+(mState-1)*cont$numStates,],na.rm=TRUE)
    #     }
    #     else {
    #       for(m in mStates) {
    #         q <- q+max(cont$utility[wNewState+(m-1)*cont$numStates,],na.rm=TRUE)
    #       }
    #       q <- q / length(mStates)
    #     }
    #     newQ <- themaze$reward + cont$gamma*q
    #     cont$utility[[utilRow, i]] <- oldQ - cont$lambda*(oldQ - newQ)
    #   }
    # }
    # 
    return(cont)
  }
  
  list(decideAction = decideAction, update = update, doRand = T)
}