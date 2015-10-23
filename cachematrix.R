## The following pair of functions cache the inverse of a matrix


## The makeCacheMatrix creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      mx <- NULL
      set <- function(y) {
      x <<- y
      mx <<- NULL
    }
      get <- function() x
      setinverse <- function(solve) mx <<- solve
      getinverse <- function() mx
      list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The function cacheSolve computes the inverse of the matrix created
## above. If the inverse is already created then the function will 
## retrieve it from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mx <- x$getinverse()
    if(!is.null(mx)) {
      message("getting cached data")
      return(mx)
  }
    matrixdata <- x$get()
    mx <- solve(matrixdata, ...)
    x$setinverse(mx)
    mx
}
