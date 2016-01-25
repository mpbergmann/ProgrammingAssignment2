## Functions to calculate the inverse of a matrix, and to store that inverse
## in a cached variable, which can be quickly retrieved should the calculation
## be attempted again. 
##
## Based on the example scripts for vector caching and mean vector calcultions
## Jan 24, 2016

## the makeCacheMatrix function defines a cache variable, and stores the inverse## matrix once it has been calculated.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


##  The cachSolve function derives the inverse of a matrix, either by pulling
##  a stored value from the cache, or by calculating it anew if necessary.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
