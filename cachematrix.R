## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## mackeCacheMatrix caches the 'x' matrix parameter
makeCacheMatrix <- function(x = matrix()) {
      ## m initizalization (only for makeCacheMatrix scope)
      m <- NULL
      ## methods to store and use makeCacheMatrix functions
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) m <<- solve
      getinverse <- function() m
      ## list of methods
      list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve returns the inverse matrix of the 'x' matrix parameter
## first of all, search for the inverse matrix in the cache
## in case the inverse matrix is already in the cache, 
##      cacheSolve gets the stored value from the cache
## in case the inverse matrix is not in the cache,
##      cacheSolve calculates the inverse matrix for 'x' 
##      and stores it to the cache
cacheSolve <- function(x, ...) {
      ## first of all, search for the inverse matrix in the cache
      ##      by using getinverse() defined in makeCacheMatrix
      m <- x$getinverse()
      ## in case the inverse matrix is already in the cache,
      ##      cacheSolve gets the stored value from the cache 
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      ## in case the inverse matrix is not in the cache,
      ##      cacheSolve calculates the inverse matrix for 'x'
      ##      and stores it to the cache 
      ##      by using setinverse() defined in makeCacheMatrix
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      ## Return a matrix that is the inverse of 'x'
      m
}
