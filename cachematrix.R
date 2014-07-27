## Define two functions makeCacheMatrix and cacheSolve. These functions are
## useful because they prevent to waste time calculating the inverse of a matrix
## that was previously solved.

## This function creates a special type of "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    # As the inverse isn't calculated yet it's set to NULL
    inverse <- NULL
    # Define set method to change the matrix and reset the inverse to NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    # Define get method to obtain the matrix
    get <- function() x
    # Define setinverse method to set the inverse of the "CacheMatrix" object
    setinverse <- function(i) inverse <<- i
    # Define getinverse method to get the inverse of the "CacheMatrix" object
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates de inverse of the matrix only if it isn't in cache.
## And returns that inverse.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    # If the inverse is not NULL then it's in the cache, so there's nothing to
    # calculate
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    # Otherwise it calculate and store it in the cache
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
