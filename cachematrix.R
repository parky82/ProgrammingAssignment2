## Two functions that together cache the inverse of a matrix

## Create a “matrix” object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <-function(y){
    x <<- y
    inv <<- NULL
    
  }
  get <- function(){x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function(){inv}
  list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
  
}


## Compute the inverse of the "matrix" returned by makeCacheMatrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("obtaining cache data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve (mat, ...)
  x$setInverse(inv)
  inv
}
