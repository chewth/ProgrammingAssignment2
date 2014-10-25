## Put comments here that give an overall description of what your
## functions do


# The function first checks if the inverse has already been computed. 
# If so, it gets the result and skips the computation. If not, it computes 
# the inverse, sets the value in the cache via setinverse function.

makeCacheMatrix <- function(x = matrix()) {
  inverse_m <- NULL
  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_m <<- inverse
  getinverse <- function() inverse_m
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# This function assumes that the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inverse_m <- x$getinverse()
  if(!is.null(inverse_m)) {
    message("Retrieving cached matrix.")
    return(inverse_m)
  }
  data <- x$get()
  inverse_m <- solve(data)
  x$setinverse(inverse_m)
  inverse_m
}

