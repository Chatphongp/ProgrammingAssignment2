## Create a Special Matrix that can store its inverse value in cache.
## The function return a list of 4 functions
## set(args) store the matrix in this special matrix
## get() return the stored matrix
## setinverse(inv) store the inverse value of the matrix
## getinverse() return the stored inverse value of the matrix
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


## A special cache function that calculate inverse of special matrix( from above function) only
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


