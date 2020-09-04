## These functions provide the capability to create a cache-able
## R matrix, and find the inverse of that matrix with improved
## speed by leveraging the cached inversion result for that
## matrix

## This function creates a matrix that is capable of
## tracking the value of its inversion for return until
## it is set to a new value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinversion <- function(inversion) i <<- inversion
  getinversion <- function() i
  list(set = set, 
       get = get,
       setinversion = setinversion,
       getinversion = getinversion)
}


## This function will return the inversion of the matrix,
## taking advantage of using the cached value if it is available

cacheSolve <- function(x, ...) {
  i <- x$getinversion()
  
  if (!is.null(i)) {
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinversion(i)
  i
}
