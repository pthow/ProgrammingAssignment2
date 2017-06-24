## These two functions impliments a system where one can reuse the
## calculation of matrix inverse.  The inverse of a matrix is cache
## after it's computed for the first time.  Any subsequent call to
## the inverse function merele retrieve the cached information.
##
## To achieve this, we need a new format to store the matrix (and
## its inverse), along with a new method for inverse computation.
##


## This function converts a matrix into the new format that caches
## its own inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## The set method reset the matrix.  It is never called in this
  ## assignment.  Why is it here I don't even.
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(i)  inv <<- i
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This is the modified version of the method solve.
## It looks for cached inverse of a matrix and returns
## that if present.  Else it calculates the inverse
## by calling solve().  And in the case of a fresh
## calculation it caches the result.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  m <- x$get()
  inv <- solve(m, ...)
  x$setInverse(inv)
  inv
}
