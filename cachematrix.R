## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the cached inverse to NULL
    
    set <- function(y) {
        x <<- y     # Assign the matrix to the parent environment
        inv <<- NULL  # Reset the cached inverse
    }
    get <- function() x  # Retrieve the matrix
    
    setInverse <- function(inverse) inv <<- inverse  # Cache the inverse
    getInverse <- function() inv  # Retrieve the cached inverse
    
    # Return a list of functions for accessing and modifying the matrix and cache
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()  # Retrieve the cached inverse, if it exists
    if (!is.null(inv)) {   # Check if the inverse is already cached
        message("getting cached data")
        return(inv)        # Return the cached inverse
    }
    mat <- x$get()         # Retrieve the matrix
    inv <- solve(mat, ...) # Compute the inverse
    x$setInverse(inv)      # Cache the inverse
    inv                    # Return the inverse
}

