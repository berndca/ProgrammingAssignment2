## Coursera R-Programming: Programming Assignment 2 (Caching)
## The functions in this assignment demonstrate how caching
## the result of a potentially expensive calculation can be
## cached. 
##
## Usage: 
## m_input <- matrix(1:4, 2, 2)
## m <- makeCacheMatrix(m_input)
## m_inverse <- cacheSolve(m)       This calculates the inverse matrix.
## m_inverse <- cacheSolve(m)       This obtains the inverse matrix from the cache.
 

## The function makeCacheMatrix creates a special matrix with functions
## set and get the value of the matrix and set and get the 
## inverse of the matrix.
## Parameter: x A two dimensional numeric invertible matrix.
## Returns: List of four access functions: set, get (for the input matrix), 
## setinverse, getinverse (for the inverted matrix).

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The function cacheSolve returns the inverted matrix. It first checks if
## an inverted matrix is in the cache, if not it calculates it.
## Parameter: x List of functions created with makeCacheMatrix
## Returns: inverted matrix

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m}
