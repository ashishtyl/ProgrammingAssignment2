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
  
  getInverse <- function() inverse
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #check matrix inverse exits
  if(!is.null(x$getInverse())){ 
    #return cached invese 
    message("Matrix in cache. Getting inverse...")
    x$getInverse()
  }
  
  #matrix not in cache 
  else {
    #calculate inverse 
    t <- solve(x$get())
    ti <- solve(t)
    
    #set in cache
    x$set(t)
    x$setInverse(ti)
    ti
  }
}
