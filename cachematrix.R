
## Create a matrix object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverseMatrix <<- solve
  getinverse <- function() inverseMatrix
  list(set = set, get = get, 
       setinverse = setinverse, getinverse = getinverse)
}


## Compute the inverse of a matrix.
## Return the cached inverse if it has already been solved.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Lazy load implentation so the calculation
  ## is done once and only if necessary.
  result <- x$getinverse()
  if (is.null(result)) {
    m <- x$get()
    result <- solve(m, ...)
    x$setinverse(result)
  }
  result
}
