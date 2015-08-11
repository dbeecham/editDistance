# Wagner-Fischer "edit distance" algoritm.
editDistance <- function(str0, str1) {
  
  # Allocate matrix.
  d <- matrix(0, nchar(str0) + 1, nchar(str1) + 1)
  
  # Edit distances to empty string are easy (just deletions) and does not change.
  d[,1] <- seq(0, nchar(str0))
  d[1,] <- seq(0, nchar(str1))
  
  # Loop over [2, i] x [2, j].
  for (i in seq(2, nchar(str0)+1)) {
    for (j in seq(2, nchar(str1)+1)) {
      
      # Characters are equal, nothing needs to be done.
      if (substr(str0, i, i) == substr(str1, j, j)) {
        d[i, j] <- d[i-1, j-1]
      }
      
      else {
        d[i, j] <- min(d[i-1, j] + 1,   # Deletion
                       d[i, j-1] + 1,   # Insertion
                       d[i-1, j-1] + 1) # Substitution
      }
    }
  }
  
  return(d[i, j])
  
}