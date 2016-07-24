## The script is used to cache the inverse of a matrix. There are 2 functions:
# 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
# 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# should retrieve the inverse from the cache

## This function receives a matrix and returns a list
# containing the following elements:
# 1. set the matrix
# 2. get the matrix
# 3. set the inverse matrix
# 4. get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function receives a matrix and return its inverse if any,
# otherwise it calculates the inverse and caches the inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if (!is.null(inverse)) {
      message("getting cached inverse")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}