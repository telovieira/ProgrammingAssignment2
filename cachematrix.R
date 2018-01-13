## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special list of objects that can cache its inverse.
# It can also verify its inversibility.
# Nevertheless, assuming that the matrix supplied is always invertible, there is a function object
# which verifies its invertibility.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  isInv <- function() class(try(solve(x),silent = TRUE))=="matrix"
  setM <- function(y)
  {
    x <<- y 
    m <<- NULL
  }
  getM <- function() x
  setInvM <- function(invM) m <<- invM
  getInvM <- function() m
  list(setM = setM, getM = getM, setInvM = setInvM, getInvM = getInvM, isInv = isInv)
}


## Write a short comment describing this function
# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated 
# (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache
# otherwise it will insert the inversed matrix into the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  if (x$isInv())
  {
    m <- x$getInvM()
    if(!is.null(m)) {
      message("getting cached invertible matrix")
      return(m)
    }
    mtx <- x$getM()
    m <- solve(mtx, ...)
    x$setInvM(m)
    m
  } else {
    message("It is not an invertible matrix")
  }
}
