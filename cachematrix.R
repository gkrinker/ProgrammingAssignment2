## This file describes two functions. 
## The makeCacheMatrix function creates a
## special kind of matrix that can be inverted
## by calling cacheSolve. 

## Creates a special kind of matrix that can
## be inverted using cacheSolve

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setInverse = setinverse,
       getInverse = getinverse)
}


## Takes a special kind of matrix returned by
## makeCacheMatrix and looks for the cached 
## inverse of that matrix. If it hasn't been
## computed before it computes it using solve()
## and caches the result for next time.

cacheSolve <- function(x, ...) {
  ## Look for the value of the cached value
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ## If not, return a matrix that is the inverse
  ## of 'x'
  data <- x$get()
  i <- solve(data)
  x$setInverse(i)
  i
}
