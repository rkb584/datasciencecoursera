## Matrix inversion is usually a costly computation and there may be some benefit to caching 
## the inverse of a matrix rather than compute it repeatedly.
##
## This program contains two functons that store a matrix and cache the inverse of that matrix

## The function makeCacheMtrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  xprime <- NULL
  set <- function(y) {
    x <<- y
    xprime <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) xprime <<- inverse
  getInverse <- function() xprime
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
}


## The function cacheSolve computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse if the matrix object already exists, the function
## retrieves the inverse from cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'        
  
  xprime <- x$getInverse()
  
  if(!is.null(xprime)) {
    message("getting cached data")
    return(xprime)
  }
  
  data <- x$get()
  xprime <- solve(data, ...)
  x$setInverse(xprime)
  xprime
  
}
