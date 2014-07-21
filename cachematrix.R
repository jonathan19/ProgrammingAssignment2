##These functions serve to cache the inverse of a matrix, in order to save time
##when the inverse is needed and has already been solved.

##The makeCacheMatrix function creates a list containing funcions to get and
##set the value of a matrix and to get and set the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
      cache <- NULL
      set <- function(y) {
            x <<- y
            cache <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) cache <<- solve
      getinverse <- function() cache
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      
}


##cacheSolve solves the inverse of the matrix you stored using makeCacheMatrix.
##If the inverse has already been solved, it returns the cached value. 
##Otherwise, it solves and caches the inverse.

cacheSolve <- function(x, ...) {
      cache <- x$getinverse()
      if(!is.null(cache)) {
            message("getting cached data")
            return(cache)
      }
      data <- x$get()
      cache <- solve(data, ...)
      x$setinverse(cache)
      cache
}
