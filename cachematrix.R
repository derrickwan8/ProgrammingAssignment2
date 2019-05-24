## ProgrammingAssignment2

## This function creates a special "matrix" object 
## that can cache its inverse.

## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL 
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  
  list(set = set, 
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.

## Compute the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of x
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matx <- x$get()
  inv <- solve(matx, ...)
  x$setInverse(inv)
  inv
}

