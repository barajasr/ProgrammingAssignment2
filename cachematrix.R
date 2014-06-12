## makeCacheMatrix parallels makeVector example from README.md.
## makeCacheMatrix provide structure/functionality for matrix 
## and it's inverse form.
## cacheSolve parallels cachemean example from README.md.
## cacheSolve provides a mechanism to eliminate redundant inverse
## matrix computation.

## makeCacheMatrix creates a special matrix containter in list form.
## List elements are get/set matrix and get/set_inverse matrix functions.
makeCacheMatrix <- function(x = matrix()) {
    x_inverse <- NULL
    
    set <- function(y) {
        x <<- y
        x_inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) x_inverse <<- inverse
    get_inverse <- function() x_inverse
    
    list(set = set,
         get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
    
}

## If inverse matrix x already computed, it is returned.
## If not computed, it is computed, cached and returned.
cacheSolve <- function(x, ...) {
    x_inverse <- x$get_inverse()
    if(!is.null(x_inverse)) {
        message("getting cached data")
        return(x_inverse)
    }
    
    x_inverse <- solve(x$get(), ...)
    x$set_inverse(x_inverse)
    x_inverse    
}