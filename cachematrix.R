## Uses library MASS for inverse function

## Creates a special matrix that has access functions to get the matrix and the cached inverse
## The set function also sets the cache to NULL
library("MASS")

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    x <<- matrix(...)
    set <- function(y) {
        minv<<-NULL
        x<<-y
    }
    get <- function() x
    getinv <- function() minv
    setinv <- function(y) minv <<- y
    list(set = set, get = get, 
         getinv = getinv, setinv = setinv)
}


# Obtains the cached inverse of the special matrix created by makeCacheMatrix
# If the inverse is not null, then it is returned, else calculates the inverse
# and sets it into the cache for subsequents calls.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    linv <- x$getinv()
    if (!is.null(linv)) {
        message ("inverse returned from cache")
        return (linv)
    }
    linv <- ginv(x$get())
    x$setinv (linv)
    linv
}
