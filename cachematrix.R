# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # inverse of the matrix
  mat_inv <- NULL
  
  set <- function(y) {
    x <<- y
    mat_inv <<- NULL
  }
  
  get <- function() {
    x
  }
  
  setMatrixInverse <- function(inverse) {
    mat_inv <<- inverse
  }
  
  getMatrixInverse <- function() {
    mat_inv
  }
  
  list(set=set, get=get, setMatrixInverse=setMatrixInverse, getMatrixInverse=getMatrixInverse)
  
}


# The following function returns the inverse of the matrix.
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getMatrixInverse()
  
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setMatrixInverse(inv)
  inv
}
