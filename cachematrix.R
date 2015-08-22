## makeCacheMatrix creates a 'special matrix,' which is really a list.

## cacheSolve searches the cache for the matrix inverse; if the inverse is found, 
##      the values are returned, if the values are not found, the values are computed.

## Install MASS (Modern Applied Statistics with S) package: 
        library(MASS)
        ## ginv is a MASS command to compute the "Generalized Inverse of a Matrix." 
        ##      See ?ginv

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return (i)
        }
        data <- x$get()
        i <- ginv(data, ...)
        x$setinverse(i)
        i
}     
