## makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Initialize cache for inverse
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Reset cache when matrix is updated
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # Function to get the inverse
  getInverse <- function() inv
  
  # Return a list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: Computes or retrieves the cached inverse of the special "matrix"
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Check if inverse is already cached
  
  # If inverse is cached, return it
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  
  # Otherwise, compute the inverse
  mat <- x$get()
  inv <- solve(mat, ...)  # Compute inverse
  x$setInverse(inv)  # Store in cache
  
  inv  # Return the inverse
}
