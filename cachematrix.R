## This file contains two functions: "makeCacheMatrix" & "cacheSolve".
## Their descriptions are below.


## "makeCacheMatrix" creates a special "matrix" object that can cache its 
## inverse. The special "matrix" object can "set" the matrix and "get" the
## matrix back. It also can "setInverse" and "getInverse" of the "set" 
## matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above and stores the answer in the cache. If the inverse 
## has already been calculated, then cacheSolve will retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
