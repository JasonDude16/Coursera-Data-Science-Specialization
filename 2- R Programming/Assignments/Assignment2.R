# The functions below create a matrix whose inverse can be cached, and then 
# computes the inverse of the matrix. If the inverse has already previously been
# calculated, the values will be retrieved from the cache.

# These functions were created by slightly modifying the makeVector and 
# cachemean templates provided.

# makeCacheMatrix Function ------------------------------------------------

## Creates a matrix object whose inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {
  
  inverse <- NULL
  
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) inverse <<- inverse 
  getInverse <- function() inverse
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}

# cacheSolve Function -----------------------------------------------------

## Solves the inverse of the matrix. Retrieves the values from cache if it has 
## previously been solved.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()
  
  if (!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  mat.data <- x$get()
  inverse <- solve(mat.data, ...)
  
  x$setInverse(inverse)
  
  return(inverse)
  
}

# Example -----------------------------------------------------------------

x <- makeCacheMatrix(matrix(1:4, 2, 2))
cacheSolve(x)