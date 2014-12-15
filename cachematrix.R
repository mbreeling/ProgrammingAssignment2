## These functions will cache the inverse of a matrix so that it only
## needs to be computed once.

## This function creates a special 'matrix' object out of the matrix x that is
## actually a list of four functions.

makeCacheMatrix <- function(x = matrix()) {
 	  m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function, when passed the 'matrix' object made out of x,
## will check to see if the inverse of x has been computed. If it has,
## it will return said inverse out of the cache, and otherwise it will
## compute the inverse and then return it.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
