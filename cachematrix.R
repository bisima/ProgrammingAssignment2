## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly.
## This program implements a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache
## its inverse. Which is really a list containing a function to
## set the value of the matrix
## get the value of the matix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        ## set the value of the matrix
        inv = NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ## get the value of the matix
        get <- function() x
        
        ## set the value of the inverse
        setInverse <- function(inverse) inv <<- inverse
        
        ## get the value of the inverse
        getInverse <- function() inv
        
        ## return list of the four functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. However, if the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        ## check whether the inverse has already been calculated and matrix has not
        ## changed. If so return the cached inverse data
        if(!is.null(inv)){
                message("getting cached inverse data")
                ## Return the cached inverse of 'x'
                return(inv)
        }else { 
                ## The inverse is not yet calculated, so we calculate it
                data <- x$get()
                inv <- solve(data, ...)
                
                ## cache the inverse
                x$setInverse(inv)
        
                ## Return a matrix that is the inverse of 'x'
                return(inv)
        }
}

## Sample usage of the application: 
## x <- matrix(rnorm(9), nrow=3)        # Create a matrix x
## mat <‐ makeCacheMatrix(x)            # Create our special matrix
## mat$get()                            # Return the matrix
## cacheSolve(mat)                      # Return the inverse
## cacheSolve(mat)                      # The 2nd call returns the cached inverse
