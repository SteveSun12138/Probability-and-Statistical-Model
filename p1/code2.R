# finds P(X1 = 2), P(X2 = 2) and P(X2 = 2|X1 = 1) in ALOHA example
sim <- function(p,q,nreps) {
  countx2eq2 <- 0
  countx2eq1 = 0
  countx2eq0 = 0
  countx1eq1 <- 0
  countx1eq2 <- 0
  countx2eq2givx1eq1 <- 0
  countx2eq1givx1eq2 = 0
  countx2eq0givx1eq1 = 0
  probceq0 = 0
  probceq1 = 0
  probceq2 = 0
  # simulate nreps repetitions of the experiment
  for (i in 1:nreps) {
    numsend <- 0 # no messages sent so far
    # simulate A and B’s decision on whether to send in epoch 1
    for (j in 1:2)
      if (runif(1) < p) numsend <- numsend + 1
    if (numsend == 1) X1 <- 1
    else X1 <- 2
    if (X1 == 2) countx1eq2 <- countx1eq2 + 1
    # now simulate epoch 2
    # if X1 = 1 then one node may generate a new message
    numactive <- X1
    if (X1 == 1 && runif(1) < q) numactive <- numactive + 1
    # send?
    if (numactive == 1)
      if (runif(1) < p) X2 = 0
      else X2 = 1
    else { # numactive = 2
      numsend <- 0
      for (i in 1:2)
        if (runif(1) < p) numsend <- numsend + 1
      if (numsend == 1) X2 <- 1
      else X2 <- 2
    }
    if (X2 == 0) countx2eq0 = countx2eq0 + 1
    if (X2 == 2) countx2eq2 <- countx2eq2 + 1
    if (X2 == 1) countx2eq1 = countx2eq1 + 1
    if (X1 == 1) { # do tally for the cond. prob.
      countx1eq1 <- countx1eq1 + 1
      if (X2 == 2) countx2eq2givx1eq1 <- countx2eq2givx1eq1 + 1 
      if (X2 == 0) countx2eq0givx1eq1 <- countx2eq0givx1eq1 + 1 
    }
    
    if (X1 == 2) {
      #countx1eq2 = countx1eq2 + 1
      if (X2 == 1) countx2eq1givx1eq2 <- countx2eq1givx1eq2 + 1
    }
    
    total = 2 * p * (1 - p) * (1 - p) * (1 - p)+ 
      2* p * (1 - p) * p * (1 - q)
    total = total + 2 * p * p * p * (1 - p)
    total = total + 2 * p * p * p * q * (1-p)
    total = total + 2 * p * q * (1 - p) * (1 - p) * (1 - p)
    
    probceq0 = 2 * p * (1 - p) * (1 - p) * (1 - p)+ 
      2* p * (1 - p) * p * (1 - q)
    probceq0 = probceq0 + 2 * p * (1 - p) * q * (1 - p) * (1 - p)
    probceq0 = probceq0 / total
    
    
    probceq1 = 2 * p * p * p * (1-p) + 2 * p * p * p * q * (1 - p)
    probceq1 = probceq1 / total
    
    xx = p * p
    
    probceq2 = 0
  }
  #get probability c = 0
  
  
  
  # print results
  #cat("P(X1 = 2):",countx1eq2/nreps,"\n") 
  #cat("P(X1 = 1):",countx1eq1/nreps,"\n")
  #cat("P(X2 = 2):",countx2eq2/nreps,"\n")
  #cat("P(X2 = 1):",countx2eq1/nreps,"\n")
  #cat("P(X2 = 0):",countx2eq0/nreps,"\n")
  #cat("P(X2 = 2 | X1 = 1):",countx2eq2givx1eq1/countx1eq1,"\n")
  #cat("P(X2 = 0 | X1 = 1):",countx2eq0givx1eq1/countx1eq1,"\n")
  #cat("P(X2 = 1 | X1 = 2):",countx2eq1givx1eq2/countx1eq2,"\n")
  cat("P(C = 0):",probceq0,"\n")
  cat("P(C = 1):",probceq1,"\n")
  cat("P(C = 2):",probceq2,"\n")
}
      
      
