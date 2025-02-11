## The following functions implement caching for matrix inversion to improve efficiency.
## Instead of computing the inverse repeatedly, the result is stored in memory and retrieved
## if the input matrix has not changed.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    set <- function(y) {
        x <<- y  # Store the matrix
        inv <<- NULL  # Reset the cached inverse
    }
    get <- function() x  # Return the matrix
    setInverse <- function(inverse) inv <<- inverse  # Store the inverse
    getInverse <- function() inv  # Return the cached inverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)  # Return a list of functions
}

## This function computes the inverse of the special "matrix" created by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then it retrieves the cached inverse to save computation time.
cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Check if inverse is already cached
    if (!is.null(inv)) {
        message("Getting cached data")  # Inform user that cached data is being used
        return(inv)
    }
    mat <- x$get()  # Retrieve the matrix
    inv <- solve(mat, ...)  # Compute the inverse
    x$setInverse(inv)  # Cache the inverse
    inv  # Return the inverse matrix
}
