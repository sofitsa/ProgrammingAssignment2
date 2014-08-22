## These functions calculate the inverse of a matrix, 
## returning its cached value in case it has already been computed, or,
## computing it in case it has not been previously computed.


## The function below takes a matrix as an input and returns a list containing functions to:
## set the value of the matrix
## get the value of the matrix
## set the inverse matrix of the original
## get the inverse matrix of the original

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseMatrix) m <<- inverseMatrix
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## The function below gives the inverse of a matrix calculated with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse and sets its value in the cache.

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

