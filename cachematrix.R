## Inverting a matrix can be a costly computation. These functions 
## create and cache a square matrix, if the inverse of the matrix has
## already been calculated the cached result is returned.

## This function creates a square matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) { 
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- solve 
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function calculates the inverse of the square matrix above
## if the result has already been calculated the cached result is returned

cacheSolve <- function(x, ...) {
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
