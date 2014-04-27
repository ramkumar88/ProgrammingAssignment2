## The following functions to defining a matrix object to support caching the inverse of a matrix
## If the solve operation fails, the error is caught and caching doesn't occur
## If the solve operation succeeds, the value is cached and the cached value is returned on next time inverse is run
##
## Example usage:
##  origMatrix <- makeCacheMatrix(diag(3,5,5))
##  invMatrix <- cacheSolve(origMatrix) ## solve inverse first time and cache
##  origMatrix$getInverse() ## get cached inverse matrix

## method: makeCacheMatrix 
## purpose: To create a special matrix with methods to cache and manipulate the original and inverse matrix.
## parameters: x        - a square matrix
## returns:    list     - list of methods to get/set the original and inverse matrix values
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## method: cacheSolve 
## purpose: To solve the inverse of the matrix defined through makeCacheMatrix once and cache it for later use.
## parameters: x    - an instance of the makeCacheMatrix method
##             ...  - Additional parameters passed to solve()
## returns:    i    - inverse of square matrix in x if solved without any errors
cacheSolve <- function(x, ...) {
    error_ind <- 0  # Error flag initialized to zero
    i <- x$getInverse()
    # Return the cached inverse if it exists
    if(!is.null(i)) {
        message("getting cached inverse matrix")
        return(i)
    }
    # original square matrix
    data <- x$get()
    # Try to solve the inverse of matrix. On error, display message on error and set error flag to 1
    i <- tryCatch(solve(data, ...)
                    , error = function(e){ 
                            error_ind <- 1 
                            message(paste("Error - Unable to solve matrix -",e$message))
                    })
    # Set Inverse only if no error occured
    if(error_ind == 0){
        x$setInverse(i)
        # Return a matrix that is the inverse of 'x'
        return(i)
    }
}
