## makeCacheMatrix takes in a matrix and exposes four functions that get
## or set two represents of the matrix (original form, inverse of matrix)
##
## cacheSolve always returns the inverse of a given matrix.
## Because matrix inversion can take a while to complete, if the matrix
## parameter is the same, we cache it in memory and return it if it already
## exists in memory.

## Take in a matrix and return getters/setters for the matrix itself
## and the inverse of the matrix

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


## Calculate the inverse of a matrix. If we have already seen this matrix,
## get the cached inversion.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}