## makeCacheMatrix creates a cachable matrix 
## which can then be used by cacheSolve function

## makeCacheMatrix function returns a list of contaning 4 functions
## set, sets the data
## get, returns the data
## setInverse, caching the inverse
## getInverse, getting the cached result

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve uses the list created in makeCacheMatrix
## first it checks if the inverse is already computed
## if not, computes the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(matrix)
  x$setInverse(inv)
  
  inv
}
