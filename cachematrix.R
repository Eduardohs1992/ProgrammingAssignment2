###################
# makeCacheMatrix #
###################

#'The "makeCacheMatrix" function creates a special matrix object, and then the "cacheSolve" function calculates the inverse of the matrix. 
#'If the inverse of the array has already been computed, it is found in the cache and returns it, with no need to recalculate it.

makeCacheMatrix <- function(x = matrix()) {
    inverse_x <- NULL
    set <- function(y) {
        x <<- y
        inverse_x <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverse_x <<- inverse
    getinverse <- function() inverse_x
    list(set = set,
         get = get,
         setinverse = setinverse ,
         getinverse = getinverse)
    
}

##############
# cacheSolve #
##############

#'The following function calculates the inverse matrix of the special "matrix" created with the above function. 
#'However, it first checks to see if the inverse matrix has already been calculated. 
#'If so, it gets the inverse matrix from the cache and skips the computation. 
#'Otherwise, it calculates the inverse matrix of the data and sets the inverse matrix in the cache via the cacheSolve function.


cacheSolve <- function(x, ...) {
    inverse_x <- x$getinverse()
    if(!is.null(inverse_x)) {
        message("Getting cached data.")
        return(inverse_x)
    }
    data <- x$get()
    inverse_x <- solve(data)
    x$setinverse(inverse_x)
    inverse_x
}