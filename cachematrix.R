## Create a "CacheMatrix" that will cache a matrix and its inverse

## makeCacheMatrix stores a given matrix, and also its inverse if the function calculated it already.
makeCacheMatrix <- function(x = matrix()) {
  ## set the matrix and its inverse to NULL for starters
  inv <- NULL  #inverse of matix
  
  
  ## create a setter that caches v, the original matrix and inv, the inverse
  set <- function(matrix) {
    x <<- matrix
    inv <<- NULL
  }
  
  ## create a getter that returns the cached original matrix, v
  get <- function() {
    x
  }
  
  ## create a getter that returns the cached inverse matrix inv
  getinv <- function() {
    inv
  }
  
  ## create a setInverse function (for debugging purposes so user can insert values to test)
  setInverse <- function(usr_inv) {
    inv <<- usr_inv
  }
  
  ## return the CacheVector object as a list of 4 functions
  list(set = set, get = get, getinv = getinv, setInverse = setInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Gets the value of the inverse
  inv <- x$getinv()
  # data <- matrix()
  
  ## Tests to see whether the the inverse has already been calculated
  if(!is.null(inv)) {
    message("Getting cached data.  This has already been calculated")
    return(inv)
  }
  
  ## Gets the original matrix
  data <- x$get()
  #as.matrix(data)
  
  ##Calculates original matrix's inverse
  inv <- solve(data)  
  
  ## Updates the inverse matrix's in variable
  x$setInverse(inv)
  inv
}