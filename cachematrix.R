## makeCacheMatrix() and cacheSolve() are functions that allow the user
## to create the inverse of a matrix once and continue to call the inverse
## in the future without having to recalculate it.
## --------------------------------------------------------------------------
## This function takes an invertible matrix and returns a list of functions
## that sets the inverse of that matrix and caches it or gets a cached version
## of it.
## --> This function assumes that the parameter supplied is an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function looks for a cached version of the matrix inverse if it is
## available or sets a cached version if one is not, using the functions 
## in the list returned by makeCacheMatrix().
## If a cached version is available, it prints the message "getting cached data".

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}