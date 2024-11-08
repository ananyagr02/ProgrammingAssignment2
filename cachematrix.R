## This code creates a special matrix object that can cache its inverse.
## It includes two functions:
## - makeCacheMatrix: Creates a special "matrix" object that can cache its inverse.
## - cacheSolve: Computes the inverse of the special "matrix" returned by makeCacheMatrix. If the inverse is already cached, it retrieves it from the cache.

## This function creates a special "matrix" object that can cache its inverse.
## It returns a list of functions to:
## - set the matrix
## - get the matrix
## - set the inverse of the matrix
## - get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse as NULL

        # Function to set a new matrix and reset the cached inverse
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset the inverse when the matrix is changed
        }

        # Function to get the current matrix
        get <- function() x

        # Function to set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse

        # Function to get the cached inverse of the matrix
        getInverse <- function() inv

        # Return a list of the above functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## This function computes the inverse of the "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## it retrieves the inverse from the cache, saving computation time.
cacheSolve <- function(x, ...) {
        ## Retrieve the cached inverse if available
        inv <- x$getInverse()
        
        # If the inverse is already cached, return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        
        # Get the matrix data
        data <- x$get()
        
        # Calculate the inverse of the matrix
        inv <- solve(data, ...)
        
        # Cache the computed inverse
        x$setInverse(inv)
        
        # Return the inverse
        inv
}
