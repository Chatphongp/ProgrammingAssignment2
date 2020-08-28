## Create a Special Matrix that can store its inverse value in cache.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## A special cache function that calculate inverse of matrix only
## if it has not been calculcated to save computation cost

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return (i)
  }
  
  d <- x$get()
  i <- solve(d, ...)
  x$setinverse(i)
  i
}


