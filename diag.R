#Artificial Intelligence Autumn 2018
#Project 3: Diagnostics
#Group 131: Gunnar Eggertsson & Jo Gay
#Please use runDiagnostics(learn=learn, diagnose=diagnose)

#Internal function to calculate the probability of a given row of data from the network
calcProb<-function(data, network)
{
  pEnd<-dnorm(data[[2]], mean=network$P1[2*data[[1]]+1], sd=network$P1[2*data[[1]]+2])
  if(data[[3]]) {
    pEnd<-pEnd*network$P2
  }
  else {
    pEnd<-pEnd*(1-network$P2)
  }
  if(data[[5]]) {
    pEnd<-pEnd*network$P3
  }
  else {
    pEnd<-pEnd*(1-network$P3)
  }
  p4Index<-1+data[[1]]+2*data[[4]]+4*data[[6]]
  if(data[[8]]) {
    pEnd<-pEnd*network$P4[[p4Index]]
  }
  else {
    pEnd<-pEnd*(1-network$P4[[p4Index]])
  }
  p5Index<-1+data[[6]]+2*data[[7]]
  if(data[[9]]) {
    pEnd<-pEnd*network$P5[[p5Index]]
  }
  else {
    pEnd<-pEnd*(1-network$P5[[p5Index]])
  }
  if(data[[1]]) {
    pEnd<-pEnd*network$P6
  }
  else {
    pEnd<-pEnd*(1-network$P6)
  }
  p7Index<-1+data[[3]]
  if(data[[4]]) {
    pEnd<-pEnd*network$P7[[p7Index]]
  }
  else {
    pEnd<-pEnd*(1-network$P7[[p7Index]])
  }
  p89Index<-1+data[[5]]
  if(data[[6]]) {
    pEnd<-pEnd*network$P8[[p89Index]]
  }
  else {
    pEnd<-pEnd*(1-network$P8[[p89Index]])
  }
  if(data[[7]]) {
    pEnd<-pEnd*network$P9[[p89Index]]
  }
  else {
    pEnd<-pEnd*(1-network$P9[[p89Index]])
  }
  return(pEnd)
}

#learn function to be used
learn<-function(historicalData)
{
  #Order of columns is:
  #1: Pneumonia, 2: Temperature, 3: VisitedTB, 4: TB, 5: Smokes, 6: LungCancer, 7: Bronchitis, 8: X-ray, 9: Dyspnea
  #The known variables are Temperature, VisitedTB, Smokes, X-ray, Dyspnea
  #The unknown variables are Pneumonia, TB, LungCancer, Bronchitis
  #We need to estimate 
  #1 P(Temperature | Pneumonia)
  #2 P(VisitedTB)
  #3 P(Smokes)
  #4 P(X-ray | Pneumonia, TB, LungCancer)
  #5 P(Dyspnea | Bronchitis, LungCancer)
  #6 P(Pneumonia)
  #7 P(TB | VisitedTB)
  #8 P(Lung Cancer | Smokes)
  #9 P(Bronchitis | Smokes)
  
  numRows<-dim(historicalData)[1]

  P1<-c(mean(historicalData[historicalData$Pn==0,]$Te), sd(historicalData[historicalData$Pn==0,]$Te),
        mean(historicalData[historicalData$Pn==1,]$Te), sd(historicalData[historicalData$Pn==1,]$Te))
  
  P2<-(1+sum(historicalData$VTB))/(numRows+2)
  P3<-(1+sum(historicalData$Sm))/(numRows+2)

  p4Lens<-c(sum(historicalData$Pn==0 & historicalData$TB==0 & historicalData$LC==0)+2,
            sum(historicalData$Pn==1 & historicalData$TB==0 & historicalData$LC==0)+2,
            sum(historicalData$Pn==0 & historicalData$TB==1 & historicalData$LC==0)+2,
            sum(historicalData$Pn==1 & historicalData$TB==1 & historicalData$LC==0)+2,
            sum(historicalData$Pn==0 & historicalData$TB==0 & historicalData$LC==1)+2,
            sum(historicalData$Pn==1 & historicalData$TB==0 & historicalData$LC==1)+2,
            sum(historicalData$Pn==0 & historicalData$TB==1 & historicalData$LC==1)+2,
            sum(historicalData$Pn==1 & historicalData$TB==1 & historicalData$LC==1)+2)
  P4<-c(1+sum(historicalData[historicalData$Pn==0 & historicalData$TB==0 & historicalData$LC==0,]$XR),
        1+sum(historicalData[historicalData$Pn==1 & historicalData$TB==0 & historicalData$LC==0,]$XR),
        1+sum(historicalData[historicalData$Pn==0 & historicalData$TB==1 & historicalData$LC==0,]$XR),
        1+sum(historicalData[historicalData$Pn==1 & historicalData$TB==1 & historicalData$LC==0,]$XR),
        1+sum(historicalData[historicalData$Pn==0 & historicalData$TB==0 & historicalData$LC==1,]$XR),
        1+sum(historicalData[historicalData$Pn==1 & historicalData$TB==0 & historicalData$LC==1,]$XR),
        1+sum(historicalData[historicalData$Pn==0 & historicalData$TB==1 & historicalData$LC==1,]$XR),
        1+sum(historicalData[historicalData$Pn==1 & historicalData$TB==1 & historicalData$LC==1,]$XR)
        )/p4Lens
  p5Lens<-c(sum(historicalData$LC==0 & historicalData$Br==0)+2,
            sum(historicalData$LC==1 & historicalData$Br==0)+2,
            sum(historicalData$LC==0 & historicalData$Br==1)+2,
            sum(historicalData$LC==1 & historicalData$Br==1)+2)
  P5<-c(1+sum(historicalData[historicalData$LC==0 & historicalData$Br==0,]$Dy),
        1+sum(historicalData[historicalData$LC==1 & historicalData$Br==0,]$Dy),
        1+sum(historicalData[historicalData$LC==0 & historicalData$Br==1,]$Dy),
        1+sum(historicalData[historicalData$LC==1 & historicalData$Br==1,]$Dy))/p5Lens

  P6<-(1+sum(historicalData$Pn))/(numRows+2)
  P7<-c((1+sum(historicalData[historicalData$VTB==0,]$TB))/(2+sum(historicalData$VTB==0)),
        (1+sum(historicalData[historicalData$VTB==1,]$TB))/(2+sum(historicalData$VTB==1)))
  
  P8<-c((1+sum(historicalData[historicalData$Sm==0,]$LC))/(2+sum(historicalData$Sm==0)),
        (1+sum(historicalData[historicalData$Sm==1,]$LC))/(2+sum(historicalData$Sm==1)))
  
  P9<-c((1+sum(historicalData[historicalData$Sm==0,]$Br))/(2+sum(historicalData$Sm==0)),
        (1+sum(historicalData[historicalData$Sm==1,]$Br))/(2+sum(historicalData$Sm==1)))
  
  list(P1=P1,P2=P2,P3=P3,P4=P4,P5=P5,P6=P6,P7=P7,P8=P8,P9=P9)
}

#diagnose function to be used
diagnose<-function(network, cases)
{
  outcomeCols<-c(1,4,6,7)
  nCases<-dim(cases)[1]
  probs<-matrix(0, nrow=nCases, ncol=length(outcomeCols))
  nSamples<-50000
  burn<-5000
  for(i in 1:nCases){
    #Generate a random outcome as a starting point
    randStart<-runif(4)>0.5
    startPt<-cases[i,]
    startPt[outcomeCols]<-randStart
    startPt<-as.numeric(startPt)
    samples<-matrix(0, nrow=nSamples-burn, ncol=9)

    #Calculate probability for this starting point
    pStart<-calcProb(startPt, network)
    
    for(s in 1:nSamples) {
      for(o in outcomeCols) {
        #Switch each outcome variable in turn, and calcuate new probability
        startPt[o]<-!startPt[o]
        pEnd<-calcProb(startPt, network)
  
        #Decide whether to keep it
        if(pEnd < pStart) {
          if(runif(1) > pEnd/pStart) {
            #discard with probability (1-pEnd/pStart)
            startPt[o]<-!startPt[o]
            pEnd<-pStart
          }
        }
        pStart<-pEnd
      }
      
      if(s > burn) {
        samples[s-burn,]<-as.numeric(startPt)
      }
    }
    #Now we have completed sampling, estimate the probabilites for the unknown variables
    probs[i,]<-colMeans(samples[,outcomeCols])
    
  }
  return(probs)
}
