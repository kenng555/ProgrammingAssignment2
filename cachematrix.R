## The following functions will allow the result of Matrix Inversion
## to be stored in cache for subsequent reference without re-computing.

## function makeCacheMatrix will create a special "matrix", which is really a 
## list containing a function to:
##
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversion
## 4. get the value of the inversion

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


## function cacheSolve will check if inversion result has been stored
## in cache.  If yes, it will return the result from cache.  If no, 
## it will perform the inversion using function solve(), store the 
## result in cache and return the result.

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
