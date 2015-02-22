## Functions to cache the inverse of a matrix in order to avoid repetead time-consuming computations.

## Object containing the cached inversed and methods to manipulate it.
makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invx <<- inv
  getinv <- function() invx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return the matrix inverse of x. If the inverse is not cached, it computes and caches the inverse.
cacheSolve <- function(x, ...) {
  invx <- x$getinv()
  if(!is.null(invx)) {
    #message("Getting cached inverse.")
    return(invx)
  }
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
  
}
