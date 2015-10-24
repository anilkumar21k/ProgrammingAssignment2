## Caching the Inverse of a Matrix:
## Matrix inversion is a costly computation.  
## Caching the inverse of a matrix saves resources required to compute the inverse repeatedly.
## The following two functions create a special object that can store a matrix and 
## caches its inverse.

## The following function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  k <- NULL
  set <- function(y) {
    x <<- y
    k <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) k <<- inverse
  getInverse <- function() k
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## The following function computes the inverse of the special "matrix" created by the
## makeCacheMatrix function described above. This function also retrives the inverse from 
## the cache provided that:
## a) the inverse has already been computed
## b) and the matrix has not been modified.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  k <- x$getInverse()
  if (!is.null(k)) {
    message("This is cached data")
    return(k)
  }
  z <- x$get()
  k <- solve(z, ...)
  x$setInverse(k)
  k
}