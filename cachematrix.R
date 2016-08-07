## makeCacheMatrix takes a matrix as input and returns a list of four functions that get or set the value 
## of the matrix and its inverse. 
## The second function cache solve uses takes the matrix created by the first function and if the 
## inverse does not exist in the cache calculated it and saves it to cache.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  setM <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  getM <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(setM = setM, getM = getM,
       setinverse = setinverse,
       getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$getM()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
