## Functions to cache the inverse of a matrix in order to avoid repetead time-consuming computations.

## Object containing the cached inversed and methods to manipulate it.
makeCacheMatrix <- function(x = matrix()) {
  invx <- NULL #When we call makeCacheMatrix, we create a new matrix. No inverse associated.
  
  #Sets the matrix.
  set <- function(y) {
    x <<- y
    invx <<- NULL
  }
  
  #Gets the matrix.
  get <- function() x
  
  #Sets the inverse.
  setinv <- function(inv) invx <<- inv
  
  #Gets the inverse.
  getinv <- function() invx
  
  #Return a list with the for methods in makeCacheMatrix.
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Return the matrix inverse of x. If the inverse is not cached, it computes and caches the inverse.
cacheSolve <- function(x, ...) {
  #Get the inverse from the CachemMatrix.
  invx <- x$getinv()
  
  #If the inverse was cached, we return it.
  if(!is.null(invx)) {
    return(invx)
  }
  
  #Otherwise, we compute the inverse, cache it, and return it.
  data <- x$get()
  invx <- solve(data, ...)
  x$setinv(invx)
  invx
}
