## These functions provide a cached implementation of a matrix.
## The inverse operation is a time consuming operation and
## hence cached i.,e stored on prior computation and returned on
## subsequent invocations.

## This function is the constructor for a matrix which has a cacheable
## inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y = matrix()) {
    ## no need to replace if identical, saves inverse computation
    ## time.
    if (!identical(x,y)) {
      x <<- y
      inv <<- NULL
    }
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function returns a matrix which is the inverse of the input.
## If the inverse is already cached, it is returned from the cache, else
## it is returned from a fresh computation.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x', either from cache if
  ## available, or from a new computation
  inv <- x$getinv()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  # Since inverse is not in cache, compute the inverse,
  # store in cache and return it
  inv <- solve(x$get())
  x$setinv(inv)
  return(inv)
}
