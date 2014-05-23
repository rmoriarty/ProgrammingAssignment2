## Implementation of CacheMatrix, a special "matrix" type that computes the
## matrix inverse upon request, and then caches the result so that subsequent
## requests need not be re-computed as long as the original matrix has not
## changed.

## Function to create a CacheMatrix instance, which is actually a list
## containing functions to (1) set the matrix, (2) get the matrix, (3) set the
## matrix inverse, and (4) get the matrix inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Return the matrix inverse of 'x'.  If the matrix has not changed since the
## last time its inverse was computed, return the cached value.  Otherwise,
## compute the matrix inverse, cache it, and return it.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
