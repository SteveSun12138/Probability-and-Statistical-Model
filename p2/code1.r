sim1 <- function(nreps){
  nstops <- 2 
  countl1eq0 <- 0 
  countl1eq1 = 0
  countl1eq2 = 0
  countl2eq0 = 0
  countl2eq1 = 0
  countl2eq2 = 0
  countl2eq3 = 0
  countl2eq4 = 0
  for (i in 1:nreps) {
    passengers2 <- 0 
    passengers1 = 0
    
    #at stop 1
    if (passengers1 > 0)
      for (k in 1:passengers1)
        if (runif(1) < 0.2) 
          passengers1 <- passengers1 - 1
          
        newpass1 <- sample(0:2,1,prob=c(0.5,0.4,0.1))
        passengers1 <- passengers1 + newpass1 
    
    if (passengers1 == 0) countl1eq0 <- countl1eq0 + 1
    if (passengers1 == 1) countl1eq1 <- countl1eq1 + 1
    if (passengers1 == 2) countl1eq2 <- countl1eq2 + 1
    
        
    #at stop 2
    for(j in 1:2) {
      if (passengers2 > 0)
        for (k in 1:passengers2)
          if (runif(1) < 0.2) 
            passengers2 <- passengers2 - 1
              
          newpass2 <- sample(0:2,1,prob=c(0.5,0.4,0.1))
          passengers2 <- passengers2 + newpass2
    }
          
    if (passengers2 == 0) countl2eq0 <- countl2eq0 + 1
    if (passengers2 == 1) countl2eq1 <- countl2eq1 + 1
    if (passengers2 == 2) countl2eq2 <- countl2eq2 + 1
    if (passengers2 == 3) countl2eq3 <- countl2eq3 + 1
    if (passengers2 == 4) countl2eq4 <- countl2eq4 + 1
          
  } 

  
  valuel1 = c(0,1,2)
  valuel1squ2 = c(0,1,4)
  probl1 = c(countl1eq0/nreps,countl1eq1/nreps,countl1eq2/nreps)
  el1 = weighted.mean(valuel1, probl1)
  valuel2 = c(0,1,2,3,4)
  valuel2squ2 = c(0,1,4,9,16)
  probl2 = c(countl2eq0/nreps,countl2eq1/nreps,countl2eq2/nreps,countl2eq3/nreps,countl2eq4/nreps)
  el2 = weighted.mean(valuel2, probl2)
  #cat("E[L1]:",el1,"\n")
  #cat("E[L2]:",el2,"\n")
  
  vl1 = var(el1)
  #cat("Var[L1]:",vl1,"\n")
  
  value2min1squ2 = c(1,4,1,1,4,4,1,1,4)
  value2min1 = c(1,2,-1,1,2,-2,-1,1,2)
  probl2minl1 = c(0.5*0.4, 0.5*0.1,0.4*0.2*0.5,0.4*(0.4*0.8+0.1*0.2),
                  0.4*0.1*0.8,0.1*(0.2*0.2*0.5),0.1*(2*0.5*0.8*0.2 + 0.4*0.2*0.2),
                  0.1*(0.4*0.8*0.8 + 2*0.1*0.2*0.8), 0.1*0.1*0.8*0.8)
  

  
  el2minl1squ2 = weighted.mean(value2min1squ2,probl2minl1) / 2
  el2minl1 = weighted.mean(value2min1, probl2minl1) / 2
  #cat("E[L2 - L1]:",el2 - el1,"\n")
  #cat("E[(L2 - L1)^2]:",el2minl1,"\n")
  
  el1squ2 = weighted.mean(valuel1squ2, probl1)
  el2squ2 = weighted.mean(valuel2squ2, probl2)
  #el2minl1squ2 = weighted.mean(value2min1, )
  
  varl1 = el1squ2 - el1 * el1
  varl2 = el2squ2 - el2 * el2
  varl2minl1 = el2minl1squ2 - el2minl1 * el2minl1
  

  
  covl1l2 = (varl1 + varl2 - varl2minl1) / 2
  
  
  cat("Var[L1]:",varl1,"\n")
  cat("Var[L2]:",varl2,"\n")
  cat("Var[L2 - l1]:",varl2minl1,"\n")
  cat("Difference:", varl1 + varl2 - varl2minl1, "\n")
  cat("Cov[L1,L2]:",covl1l2,"\n")
}

