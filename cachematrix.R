## Matrix inversion can be a costly computation, so the
## functions below are used to cache the inverse of a matrix.


## The makeCacheMatric function creates a special "matrix" 
## object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  ## Function for setting the matrix
  set <- function( m ) {
    x <<- m
    inverse <<- NULL
  }
  
  ## Function for getting the matrix
  get <- function() {
    x
  }
  
  ## Function for setting the inverse of the matrix
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  ## Function for getting the inverse of the matrix
  getInverse <- function() {
    inverse
  }
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## The cacheSolve function computes the inverse
## of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated, then the
## cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  matrix <- x$getInverse()
  ## Get cached data if it exists
  if (!is.null(matrix)) {
    message("Getting cached data")
    return(matrix)
  }
  
  data <- x$get()
  matrix <- solve(data)
  x$setInverse(matrix)
  
  ## Return matrix
  matrix
}

