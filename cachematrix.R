## Two functions are provided here:
## 1. makeCacheMatrix is a function which creates a special "matrix" object
##    that can cache its inverse.
## 2. cacheSolve takes an argument matrix (x) and returns a matrix that is the
##    inverse of x. Function assumes that the matrix x is invertible


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        setmat <<- function(y) {
                x <<- y
                m <<- NULL
        }
        getmat <<- function() x
        setinv <<- function(solve) m <<- solve
        getinv <<- function() m
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then cacheSolve will retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' first checking whether
        ## or not the inverse is cached
        m <- getinv()
        if(!is.null(m) & is.matrix(x) & is.matrix(getmat()) & all(x == getmat())) {
                message("getting cached data")
                return(m)
        }
        m <- solve(x, ...)
        setmat(x)
        setinv(m)
        m
}
