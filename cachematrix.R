###############################################################################
## "CacheMatrix.R" contains 2 functions.  
## "makeCacheMatrix" function creates a matrix object and stores its inverse if 
##  calculated.
## "cacheSolve" function creates a the inverse matrix of the matrix object 
##  created by "makeCacheMatrix"
## "makeCacheMatrix" function stores a given matrix, and also its inverse if 
##  the function calculated it already.
###############################################################################

## "makeCacheMatrix" function stores a given matrix and its inverse.  
makeCacheMatrix <- function(x = matrix()) {
  ## Initializes Inverse Matrix to NULL
  inv <- NULL  
  
  
  ## Allows the user to change the value of the original matrix, then reset its
  ## inverse to NULL
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  ## The "get" function provides the cached original matrix, x
  get <- function() {
    x
  }
  
  ## The "getinv" function provides the cached inverse matrix, inv
  getinv <- function() {
    inv
  }
  
  ## The "setInverse" function sets the value for the inv variable.
  setInverse <- function(usr_inv) {
    inv <<- usr_inv
  }
  
  ## This list allows users to access the sub-functions of makeCacheMatrix
  list(set = set, get = get, getinv = getinv, setInverse = setInverse)
}


## CacheSolve computes the inverse of the "CacheMatrix" returned by `
## makeCacheMatrix` above.

cacheSolve <- function(x, ...) {
  ## Gets the value of the inverse
  inv <- x$getinv()
   
  ## Tests to see whether the the inverse has already been calculated
  if(!is.null(inv)) {
    message("Getting cached data.  This has already been calculated")
    return(inv)
  }
  
  ## Gets the original matrix
  data <- x$get()

  
  ##Calculates original matrix's inverse
  inv <- solve(data)  
  
  ## Updates the inverse matrix's inv variable
  x$setInverse(inv)
  inv
}