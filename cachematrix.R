# The following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  # this holds the inverse of the matrix
  mat_inv <- NULL
  
  # <<- is "super assignment" operator which will assign the value at global environment
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
# Assumption is made that the supplied matrix is always invertible.

cacheSolve <- function(x, ...) {
  
  ## Returns a matrix that is the inverse of 'x'
  inv <- x$getMatrixInverse()
  
  # Return the Inverse if found in cache
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  
  # otherwise calculate Inverse and put it in cache using setMatrixInverse method
  data <- x$get()
  
  inv <- solve(data)
  x$setMatrixInverse(inv)
  inv
}

## Test Data - Start 

#> source("cachematrix.R")
#> x = rbind(c(1, 2), c(3, 4))
#> m = makeCacheMatrix(x)
#> m$get()
#[,1] [,2]
#[1,]    1    2
#[2,]    3    4
#> #first run - Should calculate the inverse and not get from cache.
#  > cacheSolve(m)
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#> 
#  > #second run should get it from cache
#  > cacheSolve(m)
#getting cached data.
#[,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5

##Test Data - End 