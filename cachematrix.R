## Matrix inversion is usually a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than 
## computing it repeatedly. These compound of functions generate 
## the inverse of a matrix from cache once it has been calculated

## This function creates a special "matrix" object that can cache
## its inverse

makeCacheMatrix <- function(x = matrix()) {
      matrixinv <- NULL
      set <- function(y) {
            x <<- y
            matrixinv <<- NULL
      }
      get <- function() x
      setmatrixinv <- function(inverse) matrixinv <<- inverse
      getmatrixinv <- function() matrixinv
      list(set = set, get = get,
           setmatrixinv = setmatrixinv,
           getmatrixinv = getmatrixinv)
}


## This function computes the inverse of the special "matrix" 
## above, If the inverse has already been calculated, then 
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      matrixinv <- x$getmatrixinv()
      if(!is.null(matrixinv)) {
            message("getting cached data")
            return(matrixinv)
      }
      data <- x$get()
      matrixinv <- solve(data, ...)
      x$setmatrixinv(matrixinv)
      matrixinv
}
