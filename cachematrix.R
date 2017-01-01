## Caching the Inverse of a Matrix.
## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly.
## makeCacheMatrix and cacheSolve are a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ##initialize i 
        i <- NULL
        
        ##getter and setter functions of matrix
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        
        ##getter and setter functions for inverse of matrix
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        
        ##return 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## If the inverse has already been calculated, it retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ##get inverse of x
        i <- x$getinverse()
        
        ##check if inverse is already  calculated
        if (!is.null(i)) {
                message("Getting cached data.")
                return(i)
        }
        
        ##if inverse not calculated, solve to find inverse
        mtdata <- x$get()
        i <- solve(mtdata , ...)
        
        ##set and return inverse
        x$setinverse(i)
        i
}
