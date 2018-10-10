# Artificial Intelligence - Project 3 - Diagnostics
# Author: Gunnar Atli Eggertsson
# Date: 2018-10-01

learn <- function(hist) {
  
  # Pneumonia
  p_pn <- matrix(nrow=1, ncol=2)
  pn0 <- 1 + (nrow(hist) - sum(hist$Pn))
  pn1 <- 1 + sum(hist$Pn)
  p_pn[1, 1] <- pn0 / (pn0 + pn1)
  p_pn[1, 2] <- pn1 / (pn0 + pn1)
  colnames(p_pn) <- c("Pn=0", "Pn=1")

  # Temperature (Normally distributed variable)
  p_te <- matrix(nrow=2, ncol=3)
  p_te[1, 1] <- 0
  p_te[2, 1] <- 1
  
  # Pn = 0
  hist_pn0 <- hist[which(hist$Pn == 0), ]
  te_mean_0 <- mean(hist_pn0$Te)
  te_sd_0 <- sd(hist_pn0$Te)
  p_te[1, 2] <- te_mean_0
  p_te[1, 3] <- te_sd_0
  
  # Pn = 1
  hist_pn1 <- hist[which(hist$Pn == 1), ]
  te_mean_1 <- mean(hist_pn1$Te)
  te_sd_1 <- sd(hist_pn1$Te)
  p_te[2, 2] <- te_mean_1
  p_te[2, 3] <- te_sd_1
  colnames(p_te) <- c("Pn", "Mean(Te)", "Sd(Te)")
 
  # Visited TB spot
  p_vtb <- matrix(nrow=1, ncol=2)
  vtb0 <- 1 + (nrow(hist) - sum(hist$VTB))
  vtb1 <- 1 + sum(hist$VTB)
  p_vtb[1, 1] <- vtb0 / (vtb0 + vtb1)
  p_vtb[1, 2] <- vtb1 / (vtb0 + vtb1)
  colnames(p_vtb) <- c("VTB=0", "VTB=1")
  
  # Smokes
  p_sm <- matrix(nrow=1, ncol=2)
  sm0 <- 1 + (nrow(hist) - sum(hist$Sm))
  sm1 <- 1 + sum(hist$Sm)
  p_sm[1, 1] <- sm0 / (sm0 + sm1)
  p_sm[1, 2] <- sm1 / (sm0 + sm1)
  colnames(p_sm) <- c("Sm=0", "Sm=1")
  
  # Tuberculosis
  p_tb <- matrix(nrow=2, ncol=3)
  p_tb[1, 1] <- 0
  p_tb[2, 1] <- 1
  # vtb = 0
  hist_vtb0 <- hist[which(hist$VTB == 0),]
  tb0_0 <- 1 + (nrow(hist_vtb0) - sum(hist_vtb0$TB))
  tb1_0 <- 1 + sum(hist_vtb0$TB)
  p_tb[1, 2] <- tb0_0 / (tb0_0 + tb1_0)
  p_tb[1, 3] <- tb1_0 / (tb0_0 + tb1_0)
  # vtb = 1
  hist_vtb1 <- hist[which(hist$VTB == 1),]
  tb0_1 <- 1 + (nrow(hist_vtb1) - sum(hist_vtb1$TB))
  tb1_1 <- 1 + sum(hist_vtb1$TB)
  p_tb[2, 2] <- tb0_1 / (tb0_1 + tb1_1)
  p_tb[2, 3] <- tb1_1 / (tb0_1 + tb1_1)
  colnames(p_tb) <- c("VTB", "TB=0", "TB=1")
 
  # Lung Cancer
  p_lc <- matrix(nrow=2, ncol=3)
  p_lc[1, 1] <- 0
  p_lc[2, 1] <- 1
  # sm = 0
  hist_sm0 <- hist[which(hist$Sm == 0),]
  lc0_0 <- 1 + (nrow(hist_sm0) - sum(hist_sm0$LC))
  lc1_0 <- 1 + sum(hist_sm0$LC)
  p_lc[1, 2] <- lc0_0 / (lc0_0 + lc1_0)
  p_lc[1, 3] <- lc1_0 / (lc0_0 + lc1_0)
  # sm = 1
  hist_sm1 <- hist[which(hist$Sm == 1),]
  lc0_1 <- 1 + (nrow(hist_sm1) - sum(hist_sm1$LC))
  lc1_1 <- 1 + sum(hist_sm1$LC)
  p_lc[2, 2] <- lc0_1 / (lc0_1 + lc1_1)
  p_lc[2, 3] <- lc1_1 / (lc0_1 + lc1_1)
  colnames(p_lc) <- c("Sm", "LC=0", "LC=1")
  
  # Bronchitis
  p_br <- matrix(nrow=2, ncol=3)
  p_br[1, 1] <- 0
  p_br[2, 1] <- 1
  # sm = 0
  br0_0 <- 1 + (nrow(hist_sm0) - sum(hist_sm0$Br))
  br1_0 <- 1 + sum(hist_sm0$Br)
  p_br[1, 2] <- br0_0 / (br0_0 + br1_0)
  p_br[1, 3] <- br1_0 / (br0_0 + br1_0)
  # sm = 1
  br0_1 <- 1 + (nrow(hist_sm1) - sum(hist_sm1$Br))
  br1_1 <- 1 + sum(hist_sm1$Br)
  p_br[2, 2] <- br0_1 / (br0_1 + br1_1)
  p_br[2, 3] <- br1_1 / (br0_1 + br1_1)
  colnames(p_br) <- c("Sm", "Br=0", "Br=1")
  
  # Dyspnea
  p_dy <- matrix(nrow=4, ncol=4)
  p_dy[1, 1] <- 0; p_dy[1, 2] <- 0; p_dy[2, 1] <- 0; p_dy[2, 2] <- 1; p_dy[3, 1] <- 1; p_dy[3, 2] <- 0; p_dy[4, 1] <- 1; p_dy[4, 2] <- 1
  # lc = 0 && br = 0
  hist_lc0_br0 <- hist[which(hist$LC == 0 & hist$Br == 0),]
  dy0_0_0 <- 1 + (nrow(hist_lc0_br0) - sum(hist_lc0_br0$Dy)); dy1_0_0 <-  1 + sum(hist_lc0_br0$Dy); p_dy[1, 3] <- dy0_0_0 / (dy0_0_0 + dy1_0_0); p_dy[1, 4] <- dy1_0_0 / (dy0_0_0 + dy1_0_0);
  # lc = 0 && br = 1
  hist_lc0_br1 <- hist[which(hist$LC == 0 & hist$Br == 1),]
  dy0_0_1 <- 1 + (nrow(hist_lc0_br1) - sum(hist_lc0_br1$Dy)); dy1_0_1 <- 1 + sum(hist_lc0_br1$Dy); p_dy[2, 3] <- dy0_0_1 / (dy0_0_1 + dy1_0_1); p_dy[2, 4] <- dy1_0_1 / (dy0_0_1 + dy1_0_1);
  # lc = 1 && br = 0
  hist_lc1_br0 <- hist[which(hist$LC == 1 & hist$Br == 0),]
  dy0_1_0 <- 1 + (nrow(hist_lc1_br0) - sum(hist_lc1_br0$Dy)); dy1_1_0 <- 1 + sum(hist_lc1_br0$Dy); p_dy[3, 3] <- dy0_1_0 / (dy0_1_0 + dy1_1_0); p_dy[3, 4] <- dy1_1_0 / (dy0_1_0 + dy1_1_0);
  # lc = 1 && br = 1
  hist_lc1_br1 <- hist[which(hist$LC == 1 & hist$Br == 1),]
  dy0_1_1 <- 1 + (nrow(hist_lc1_br1) - sum(hist_lc1_br1$Dy)); dy1_1_1 <- 1 + sum(hist_lc1_br1$Dy); p_dy[4, 3] <- dy0_1_1 / (dy0_1_1 + dy1_1_1); p_dy[4, 4] <- dy1_1_1 / (dy0_1_1 + dy1_1_1);
  colnames(p_dy) <- c("LC", "Br", "Dy=0", "Dy=1")

  # X-Ray
  p_xr <- matrix(nrow=8, ncol=5)
  p_xr[1, 1] <- 0; p_xr[1, 2] <- 0; p_xr[1, 3] <- 0; p_xr[2, 1] <- 0; p_xr[2, 2] <- 0; p_xr[2, 3] <- 1 
  p_xr[3, 1] <- 0; p_xr[3, 2] <- 1; p_xr[3, 3] <- 0; p_xr[4, 1] <- 0; p_xr[4, 2] <- 1; p_xr[4, 3] <- 1
  p_xr[5, 1] <- 1; p_xr[5, 2] <- 0; p_xr[5, 3] <- 0; p_xr[6, 1] <- 1; p_xr[6, 2] <- 0; p_xr[6, 3] <- 1
  p_xr[7, 1] <- 1; p_xr[7, 2] <- 1; p_xr[7, 3] <- 0; p_xr[8, 1] <- 1; p_xr[8, 2] <- 1; p_xr[8, 3] <- 1
  
  # Pn = 0, TB = 0, LC = 0
  hist_pn0_tb0_lc0 <- hist[which(hist$Pn == 0 & hist$TB == 0 & hist$LC == 0),]
  xr0_0_0_0 <- 1 + (nrow(hist_pn0_tb0_lc0) - sum(hist_pn0_tb0_lc0$XR)); xr1_0_0_0 <- 1 + sum(hist_pn0_tb0_lc0$XR);  
  p_xr[1, 4] <- xr0_0_0_0 / (xr0_0_0_0 + xr1_0_0_0); p_xr[1, 5] <- xr1_0_0_0 / (xr0_0_0_0 + xr1_0_0_0);
  
  # Pn = 0, TB = 0, LC = 1
  hist_pn0_tb0_lc1 <- hist[which(hist$Pn == 0 & hist$TB == 0 & hist$LC == 1),]
  xr0_0_0_1 <- 1 + (nrow(hist_pn0_tb0_lc1) - sum(hist_pn0_tb0_lc1$XR)); xr1_0_0_1 <- 1 + sum(hist_pn0_tb0_lc1$XR);  
  p_xr[2, 4] <- xr0_0_0_1 / (xr0_0_0_1 + xr1_0_0_1); p_xr[2, 5] <- xr1_0_0_1 / (xr0_0_0_1 + xr1_0_0_1);
  
  # Pn = 0, TB = 1, LC = 0
  hist_pn0_tb1_lc0 <- hist[which(hist$Pn == 0 & hist$TB == 1 & hist$LC == 0),]
  xr0_0_1_0 <- 1 + (nrow(hist_pn0_tb1_lc0) - sum(hist_pn0_tb1_lc0$XR)); xr1_0_1_0 <- 1 + sum(hist_pn0_tb1_lc0$XR);  
  p_xr[3, 4] <- xr0_0_1_0 / (xr0_0_1_0 + xr1_0_1_0); p_xr[3, 5] <- xr1_0_1_0 / (xr0_0_1_0 + xr1_0_1_0);
  
  # Pn = 0, TB = 1, LC = 1
  hist_pn0_tb1_lc1 <- hist[which(hist$Pn == 0 & hist$TB == 1 & hist$LC == 1),]
  xr0_0_1_1 <- 1 + (nrow(hist_pn0_tb1_lc1) - sum(hist_pn0_tb1_lc1$XR)); xr1_0_1_1 <- 1 + sum(hist_pn0_tb1_lc1$XR);  
  p_xr[4, 4] <- xr0_0_1_1 / (xr0_0_1_1 + xr1_0_1_1); p_xr[4, 5] <- xr1_0_1_1 / (xr0_0_1_1 + xr1_0_1_1);
  
  # Pn = 1, TB = 0, LC = 0
  hist_pn1_tb0_lc0 <- hist[which(hist$Pn == 1 & hist$TB == 0 & hist$LC == 0),]
  xr0_1_0_0 <- 1 + (nrow(hist_pn1_tb0_lc0) - sum(hist_pn1_tb0_lc0$XR)); xr1_1_0_0 <- 1 + sum(hist_pn1_tb0_lc0$XR);  
  p_xr[5, 4] <- xr0_1_0_0 / (xr0_1_0_0 + xr1_1_0_0); p_xr[5, 5] <- xr1_1_0_0 / (xr0_1_0_0 + xr1_1_0_0);
  
  # Pn = 1, TB = 0, LC = 1
  hist_pn1_tb0_lc1 <- hist[which(hist$Pn == 1 & hist$TB == 0 & hist$LC == 1),]
  xr0_1_0_1 <- 1 + (nrow(hist_pn1_tb0_lc1) - sum(hist_pn1_tb0_lc1$XR)); xr1_1_0_1 <- 1 + sum(hist_pn1_tb0_lc1$XR);  
  p_xr[6, 4] <- xr0_1_0_1 / (xr0_1_0_1 + xr1_1_0_1); p_xr[6, 5] <- xr1_1_0_1 / (xr0_1_0_1 + xr1_1_0_1);
  
  # Pn = 1, TB = 1, LC = 0
  hist_pn1_tb1_lc0 <- hist[which(hist$Pn == 1 & hist$TB == 1 & hist$LC == 0),]
  xr0_1_1_0 <- 1 + (nrow(hist_pn1_tb1_lc0) - sum(hist_pn1_tb1_lc0$XR)); xr1_1_1_0 <- 1 + sum(hist_pn1_tb1_lc0$XR);  
  p_xr[7, 4] <- xr0_1_1_0 / (xr0_1_1_0 + xr1_1_1_0); p_xr[7, 5] <- xr1_1_1_0 / (xr0_1_1_0 + xr1_1_1_0);
  
  # Pn = 1, TB = 1, LC = 1
  hist_pn1_tb1_lc1 <- hist[which(hist$Pn == 1 & hist$TB == 1 & hist$LC == 1),]
  xr0_1_1_1 <- 1 + (nrow(hist_pn1_tb1_lc1) - sum(hist_pn1_tb1_lc1$XR)); xr1_1_1_1 <- 1 + sum(hist_pn1_tb1_lc1$XR);  
  p_xr[8, 4] <- xr0_1_1_1 / (xr0_1_1_1 + xr1_1_1_1); p_xr[8, 5] <- xr1_1_1_1 / (xr0_1_1_1 + xr1_1_1_1);
  colnames(p_xr) <- c("Pn", "TB", "LC", "XR=0", "XR=1")
  
  network <- list(p_pn, p_te, p_vtb, p_tb, p_sm, p_lc, p_br, p_xr, p_dy)
  
  print("------------------------------------")
  return(network)
}

compute_probs <- function(samples, network) {
  # Pneumonia & Temperature
  if (samples[1] == 0) {
    p_pn <- network[[1]][1]
    p_te <- dnorm(samples[2], mean=network[[2]][1, 2], sd=network[[2]][1, 3])
  }
  else {
    p_pn <- network[[1]][2]
    p_te <- dnorm(samples[2], mean=network[[2]][2, 2], sd=network[[2]][2, 3])
  }
  
  
  # Visited a TB spot
  # If VTB = 0
  if (samples[3] == 0) {
    p_vtb <- network[[3]][1]
    # If TB = 0
    if (samples[4] == 0) {
      p_tb <- network[[4]][1, 2]
    }
    # If TB = 1
    else {
      p_tb <- network[[4]][1, 3]
    }
  }
  # If VTB = 1
  else {
    p_vtb <- network[[3]][2]
    # If TB = 0
    if (samples[4] == 0) {
      p_tb <- network[[4]][2, 2]
    }
    # If TB = 1
    else {
      p_tb <- network[[4]][2, 3]
    }
  }
  
  # Smoker, Lunc Cancer & Bronchitis
  # If Sm = 0
  if (samples[5] == 0) {
    p_sm <- network[[5]][1]
    
    # If LC = 0
    if (samples[6] == 0) {
      p_lc <- network[[6]][1, 2]
    }
    # If LC = 1
    else {
      p_lc <- network[[6]][1, 3]
    }
    
    # If Br = 0
    if (samples[7] == 0) {
      p_br <- network[[7]][1, 2]
    }
    # If Br = 1
    else {
      p_br <- network[[7]][1, 3]
    }
  }
  
  # If Sm = 1
  else {
    p_sm <- network[[5]][2]
    # If LC = 0
    if (samples[6] == 0) {
      p_lc <- network[[6]][2, 2]
    }
    # If LC = 1
    else {
      p_lc <- network[[6]][2, 3]
    }
    
    # If Br = 0
    if (samples[7] == 0) {
      p_br <- network[[7]][2, 2]
    }
    # If Br = 1
    else {
      p_br <- network[[7]][2, 3]
    }
  }
  
  # X-ray
  if (samples[1] == 0 && samples[4] == 0 && samples[6] == 0) {
    if (samples[8] == 0) {
      p_xr <- network[[8]][1, 4]
    }
    else {
      p_xr <- network[[8]][1, 5]
    }
  } else if (samples[1] == 0 && samples[4] == 0 && samples[6] == 1) {
    if (samples[8] == 0) {
      p_xr <- network[[8]][2, 4]
    }
    else {
      p_xr <- network[[8]][2, 5]
    }
  } else if (samples[1] == 0 && samples[4] == 1 && samples[6] == 0) {
    if (samples[8] == 0) {
      p_xr <- network[[8]][3, 4]
    }
    else {
      p_xr <- network[[8]][3, 5]
    }
  } else if (samples[1] == 0 && samples[4] == 1 && samples[6] == 1) {
    if (samples[8] == 0) {
      p_xr <- network[[8]][4, 4]
    }
    else {
      p_xr <- network[[8]][4, 5]
    }
  } else if (samples[1] == 1 && samples[4] == 0 && samples[6] == 0) {
    if (samples[8] == 0) {
      p_xr <- network[[8]][5, 4]
    }
    else {
      p_xr <- network[[8]][5, 5]
    }
  } else if (samples[1] == 1 && samples[4] == 0 && samples[6] == 1) {
    if (samples[8] == 0) {
      p_xr <- network[[8]][6, 4]
    }
    else {
      p_xr <- network[[8]][6, 5]
    }
  } else if (samples[1] == 1 && samples[4] == 1 && samples[6] == 0) {
    if (samples[8] == 0) {
      p_xr <- network[[8]][7, 4]
    }
    else {
      p_xr <- network[[8]][7, 5]
    }
  } else {
    if (samples[8] == 0) {
      p_xr <- network[[8]][8, 4]
    }
    else {
      p_xr <- network[[8]][8, 5]
    }
  }
  
  # Dyspnea
  if (samples[6] == 0 && samples[7] == 0) {
    if (samples[9] == 0) {
      p_dy <- network[[9]][1, 3]
    } 
    else {
      p_dy <- network[[9]][1, 4]
    }
  } else if (samples[6] == 0 && samples[7] == 1) {
    if (samples[9] == 0) {
      p_dy <- network[[9]][2, 3]
    } 
    else {
      p_dy <- network[[9]][2, 4]
    }
  } else if (samples[6] == 1 && samples[7] == 0) {
    if (samples[9] == 0) {
      p_dy <- network[[9]][3, 3]
    } 
    else {
      p_dy <- network[[9]][3, 4]
    }
  } else {
    if (samples[9] == 0) {
      p_dy <- network[[9]][4, 3]
    } 
    else {
      p_dy <- network[[9]][4, 4]
    }
  }
  p_tot <- p_pn * p_te * p_vtb * p_tb * p_sm * p_lc * p_br * p_xr * p_dy
  return(p_tot)
} 

diagnose <- function(network, cases) {
  
  # Number of samples
  nr_samples <- 10000
  
  # Number of cases
  nr_cases <- nrow(cases)

  # Final matrix of probabilities
  final_probs <- matrix(nrow = 10, ncol = 4)
  
  # Column indices of unknown variables
  U <- c(1, 4, 6, 7)
  
  for (k in 1:nr_cases) {
    
    # Insert the known values
    samples <- matrix(nrow = nr_samples, ncol = length(cases))
    samples[, 2] <- cases[k, 2]
    samples[, 3] <- cases[k, 3]
    samples[, 5] <- cases[k, 5]
    samples[, 8] <- cases[k, 8]
    samples[, 9] <- cases[k, 9]
    
    # Assign random values to all the unknown variables
    samples[1, 1] <- sample(0:1, 1)
    samples[1, 4] <- sample(0:1, 1)
    samples[1, 6] <- sample(0:1, 1)
    samples[1, 7] <- sample(0:1, 1) 
    
    # Compute the probability for the initial sample
    p_old <- compute_probs(samples[1, ], network)
    
    # Metropolis in Gibbs algorithm
    for (j in 1:(nr_samples-1)) {
      init <- samples[j, ] 
      
      for (i in U) {
        
        old <- samples[j, i]
        if (old == 0) {
          new <- 1
        } else {
          new <- 0
        }
        
        samples[j, i] <- new
        
        # Compute the new probability
        p_new <- compute_probs(samples[j, ], network)

        # Check if we accept the new value
        if (p_new < p_old) {
          rand <- runif(1)
          if (rand > (p_new / p_old)) {
            samples[j, i] <- old
            p_new <- p_old
          }
        } 
        p_old <- p_new
      }
      samples[j+1, ] <- samples[j, ]
      samples[j, ] <- init
    }
    # Burn period
    samples <- samples[round(nrow(samples)*0.1):nrow(samples), ]
    
    # Probability of pneumonia
    prob_pn <- sum(samples[, 1]) / nrow(samples)
    
    # Probability of tuberculosis
    prob_tb <- sum(samples[, 4]) / nrow(samples)
    
    # Probability of Lung Cancer
    prob_lc <- sum(samples[, 6]) / nrow(samples)
    
    # Probability of Bronchitis
    prob_br <- sum(samples[, 7]) / nrow(samples)
    
    final_probs[k, 1] <- prob_pn
    final_probs[k, 2] <- prob_tb
    final_probs[k, 3] <- prob_lc
    final_probs[k, 4] <- prob_br
    
  }
  
  return(final_probs)  
}



environment(learn) <- asNamespace('Diagnostics')



