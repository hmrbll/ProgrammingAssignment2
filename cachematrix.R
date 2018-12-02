## These functions will be useful for avoding loops while inverting a matrix that those content is not changing.

## Creates a special matrix that stores cached data for its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() {
                x
        }
        setinverse <- function(solve) {
                s <<- solve
        }
        getinverse <- function() {
                s
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## Calculates inverse of an matrix after checking its cached solutions.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinverse(s)
        s
}