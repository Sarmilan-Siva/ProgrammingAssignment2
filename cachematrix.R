## This function creates a special "matrix" and cashes its inverse as inverse is a costly computation.
## and if the inverse has already been calculated, it gets the inverse from the cache and skips the computation.


## This function creates a special "matrix" object that caches its inverse.
## and which stores 04 functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by the above function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  
  ## Checks whether the inverse has already been calculated
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matr <- x$get()
  m <- solve(matr, ...)
  x$setinverse(m)
  m
}
