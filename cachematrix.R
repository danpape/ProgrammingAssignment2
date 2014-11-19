## The cacheSolve function will compute the inverse of a matrix stored in a 
## cacheMatrix. It will store the inverse in the cache object in case the 
## inverse is computed again. 

## Creates a 'cacheMatrix' which is a special list containing the matrix, a
## possibly cached inverse of that matrix, and some getters and setters for
## the matrix and inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Initialize cached inverse to NULL
    inv <- NULL
    # Function to set the matrix, and reset cached inverse to NULL
    set <- function(y) {
        x <- y
        inv <<- NULL
    }
    # Gets the matrix
    get <- function() x
    # Sets the cached inverse
    setinv <- function(i) inv <<- i
    # Returns the cached inverse
    getinv <- function() inv
    
    list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Compute the inverse of the matrix contained in the cacheMatrix x. If the
## inverse has already been computed, return the cached value.

cacheSolve <- function(x, ...) {
    # Check that given argument is a cacheMatrix
    if(class(x) != "list" || !is.function(x$getinv)) {
        message("Argument is not a cacheMatrix")
        return()
    }
    # Return cached inverse if it exists
    inv = x$getinv()
    if(!is.null(inv)) {
        message("get cached data")
        return(inv)
    }
    # Otherwise, compute inverse, cache it, and return it
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
