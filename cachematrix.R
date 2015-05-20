## The makeCacheMatrix and cacheSolve functions together enable
## caching of the inverse of a matrix. Use makeCacheMatrix to
## create and cache a matrix. Use cacheSolve to get the inverse
## of a matrix created with makeCacheMatrix; the result is from
## the cache if available.

## makeCacheMatrix creates a matrix with functions to set and
## get the matrix, and also to set and get its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    invisible(list(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        ## Return a cached matrix that is the inverse of 'x'
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    ## Return a not-previously-cachedmatrix that is the inverse of 'x'
    i
}
