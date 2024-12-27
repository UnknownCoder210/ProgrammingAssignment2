## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
   inv <- NULL  # Initialize the cache for the inverse
  
  # Setter function to set the matrix and clear the cached inverse
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # Getter function to get the matrix
  get <- function() x
  
  # Setter function to set the inverse in the cache
  setInverse <- function(inverse) inv <<- inverse
  
  # Getter function to retrieve the cached inverse
  getInverse <- function() inv
  
  # Return a list of functions to access the matrix and its cached inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function
## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        # Check if the inverse is already cached
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)  # Return the cached inverse
  }
  
  # If not cached, compute the inverse
  mat <- x$get()  # Retrieve the matrix
  inv <- solve(mat, ...)  # Compute the inverse using `solve`
  x$setInverse(inv)  # Cache the computed inverse
  
  inv  # Return the inverse
}
