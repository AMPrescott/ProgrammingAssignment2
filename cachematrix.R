##  These functions allow for storing a matrix and storing the inverse in cache

## makeCacheMatrix creates a matrix and with an inverse that can be stored with it.  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Checks to see if an inverse has already been calculated for the cacheMatrix x.  If it has, retrieves
## this value.  Otherwise, it calculates the inverse and stores it the the cacheMatrix x.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
      message('getting cached data')
      return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
