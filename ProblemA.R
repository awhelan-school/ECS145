# Overloaded Index Operator
"[<-.ascendNums" <- function(msg = 'read-only', i, value){
  stop('read-only')
}

# Overloaded Addition Operator into Merge
"+.ascendNums" <- function(lhs, rhs){
  
  iBegin <- 1
  if(lhs[1] < rhs[1]){
    join <- c(lhs, rhs)
    iMid <- length(lhs) + 1
  }
  else{
    join <- c(rhs, lhs)
    iMid <- length(rhs) + 1
  }
  
  i <- iBegin
  j <- iMid
  iEnd <- length(join)
  out <- vector(length = iEnd)
  
  # Iterate over joined sequence
  for(k in 1:length(join)){
    
    # left head exists and smaller
    if(i < iMid && (j > iEnd || join[i] <= join[j])){
      out[k] = join[i]
      i = i + 1
    }
    else{
      out[k] = join[j]
      j = j + 1
    }
  }
  
  out <- makeAscendNums(out)
  return(out)
}

# Constructor 
makeAscendNums <- function(x){
  
  
  if(length(x) == 0)
    stop("Error: Undefined Operation")
  
  # Instance Variable 
  obj <- as.vector(x)
  # Compute Difference among consecutive elements
  diff <- x[-1] - x[-length(x)]
  
  
  if(length(x) > 1){
    
    attr(obj,'strictAscend') <- all(diff > 0)
  }
  else{
    attr(obj,'strictAscend') <- F
  }
  
  
  if (any(diff < 0)){
    stop('not nondecreasing')
  }
  
  class(obj) <- 'ascendNums'
  return(obj)
  
}


x <- c(-4,99)
xan <- makeAscendNums(x)


z <- c(-4,12,15,17)
zan <- makeAscendNums(z)

out <- xan + zan

print(out)



