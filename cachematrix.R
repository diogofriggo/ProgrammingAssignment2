## These two functions work together to avoid repeating computationally intensive code
## by caching the result once and retrieving it back when needed instead of recalculating it

## Returns an object (essentially a list) that can hold state
## It holds a matrix and its inverse, both values can be set and retrieved

makeCacheMatrix <- function(m = matrix()) {
        inv <- NULL
        set <- function(x) {
            m <<- x
            inv <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, 
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## It takes a especial matrix made from makeCacheMatrix and returns its cached inverse
## This functions assumes that the matrix m is always invertible
cacheSolve <- function(m, ...) {
    inv <- m$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    inv <- solve(m$get(), ...)
    m$setinverse(inv)
    inv
}
