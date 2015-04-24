## Functions  in this file defines the functions to set & get the 
## matrix and it's inverse. Matrix inverse computation is costly
## operation and hence it is cached.

## makeCacheMatrix function defines and stores list of get/set functions
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setSolved <- function(inverse) inv <<- inverse
    getSolved <- function() inv
    
    ## store get/set functions in  list
    list(set=set, get=get,
         setSolved=setSolved,
         getSolved=getSolved)
}


## cacheSolve function returns cached inverse of the matrix if cached;
## else it computes the inverse, sets and returns.
cacheSolve <- function(x, ...) {
    ## Check an return inverse if cached
    inv <- x$getSolved()
    if (!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    
    ## Compute the inverse, store and return
    data <- x$get()
    inv <- solve(data, ...) # use solve function to compute inverse
    x$setSolved(inv)
    inv
}
