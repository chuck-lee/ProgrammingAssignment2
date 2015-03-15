## makeCacheMatrix() wraps matrix object and give it's ability to cache its
## inverse.
## cacheSolve() can be used to get inverse of matrix, and update it if the
## matrix is changed after the inverse is cached.

###############################################################################
##
## makeCacheMatrix(x)
##
## This function creates a special "matrix" object that can cache its
## inverse.
##
## Input:
##   x: An invertible matrix.
##
## Output:
##   Special matrix object can cache its inverse, and determine if it's
##   changed since inverse is calculated.
##
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
    ## inverse cache
    inverse <- NULL

    ## matrix object getter and setter
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x

    ## inverse getter and setter
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse

    ## wrapped object
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


###############################################################################
##
## cacheSolve(x)
##
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix() above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
##
## Input:
##   x: wrapped matrix object returned by makeCacheMatrix.
##
## Output:
##   Inverse matrix of x.
##
###############################################################################
cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }

    message("calculate inverse and cache it")
    inverse <- solve(x$get())
    x$setinverse(inverse)
    inverse
}
