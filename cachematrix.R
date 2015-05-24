## The following two functions allow the computation of the inverse of a matrix
## and the retrieval of a computed inverse if it is already in the cache

## makeCacheMatrix takes a matrix argument x and defines a series of functions
## to get, set, find the inverse and set the inverse of that matrix x

makeCacheMatrix <- function(x = matrix()) {
  m = NULL
  set = function(y) {
    x <<- y
    m <<- NULL
  }
  get = function() x
  setinverse = function(inverse) {
    m <<- inverse
  }
  getinverse = function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## cacheSolve takes the output of makeCacheMatrix as an argument and either 
## computes and caches the inverse if it hadn't already been done so, or 
## retrieves the inverse from the cache otherwise.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m = x$getinverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data = x$get()
  m = solve(data)
  x$setinverse(m)
  m
}