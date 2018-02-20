"[<-.ascendNums" <- function(msg = 'read-only', i, value){
  stop('read-only')
}

makeAscendNums <- function(x){
  
  obj <- x
  
  # Compute Difference among consecutive elements
  diff <- x[-1] - x[-length(x)]
  attr(obj,'strictAscend') <- all(diff > 0)
  
  if (attr(obj,'strictAscend') == F){
    stop('not nondecreasing')
  }
  
  class(obj) <- 'ascendNums'
  return(obj)
  
}