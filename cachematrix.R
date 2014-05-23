## Functions that cache the inverse of a matrix

## Function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        S <- NULL
        set <- function(y) {
              x <<- y
              S <<- NULL
        }
        get <- function() x
        setSolve <- function(solve) S <<- solve
        getSolve <- function() S
        list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        S <- x$getSolve()
        if(!is.null(S)) {
              message("getting cached data")
              return(S)
        }
        data <- x$get()
        S <- solve(data, ...)
        x$setSolve(S)
        S
}
