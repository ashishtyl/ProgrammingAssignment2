makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# Set the matrix and its inverse to the environment variable, 

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  #Setting a new matrix to the parent environment 
  set <- function(y){
    x<<- y 
    inverse <<- NULL 
  }
  
  get <- function()x 
  
  setInverse <- function(inv) inverse <<- inv
  
  getInverse <- function() inv
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get cached matrix   
  i <- x$get 
  #check if cached matrix matches matrix supplied
  if(i==x){
    #return cached invese 
    message("Matrix in cache. Getting inverse...")
    return(x$getInverse)
  }
  #matrix not in cache 
  else {
    #calculate inverse 
    t <- solve(x$get)
    ti <- solve(t)
    
    #set in cache
    x$set(t)
    x$setInverse(ti)
  
    ti
  }
}
