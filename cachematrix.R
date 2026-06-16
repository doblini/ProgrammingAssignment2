## makeCacheMatrix makes a "matrix" objec that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y    #store a new matrix in the parent environment
    inv <<- NULL    # reset cached inverse when matrix changes
  }
  get <- function() x    # return new stored matrix
  setInverse <- function(inverse) inv <<- inverse    # cache  inverse
  getInverse <- function() inv   # retrieve inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve retrives the cached inverse of the matrix if it has been solved.
## If it hasn't been solved, it computes the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    return(inv)
  }
  mat <- x$get()          # get matrix
  inv <- solve(mat, ...)  # compute the inverse
  x$setInverse(inv)       # cache the inverse for future use
  inv
}
