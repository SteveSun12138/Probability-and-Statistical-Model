cards <- function(nreps) {
  count3diamonds <- 0  
  for (rep in 1:nreps) {
    hand <- sample(1:52,5,replace=FALSE)  
    diamonds <- intersect(1:13,hand)  
    if (length(diamonds) == 3) count3diamonds <- count3diamonds + 1
  }
  count2access <- 0  
  for (rep in 1:nreps) {
    hand <- sample(1:52,5,replace=FALSE)  
    access <- intersect(1:4,hand)  
    if (length(access) == 2) count2access <- count2access + 1
  }
  cat("P(2 access) = ", count2access/nreps, "\n")
  cat("P(3 diamonds) = ", count3diamonds/nreps, "\n")
}
