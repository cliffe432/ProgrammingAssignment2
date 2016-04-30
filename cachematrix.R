## The two functions below, "makeCacheMatrix" and "cacheSolve," create an object that 
## will store a matrix then cache the inverse of said matrix. The idea is to reduce the
## cost of having to run the computation repeatedly.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  invert <- NULL
  set <- function(y) {
      x <<- y
      invert <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invert <<- inverse
  getInverse <- function() invert
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  }


## The cacheSolve function computes the inverse of the special “matrix” returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invert <- x$getInverse()
  if (!is.null(invert)){
    message("cached data")
    return(inv)
  }
  mat <- x$get()
  invert <- solve(mat, ...)
  x$setInverse(invert)
  invert
  }
