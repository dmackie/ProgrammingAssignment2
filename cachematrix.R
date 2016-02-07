## The following two functions are used to allow the caching of the result of
##  inversing a matrix
##
## Code based on given examples for caching vector mean in assignment
## https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping


## Create a special matrix object that can cache the results of inverting it

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL #inverse matrix store

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


## Calculate the inverse of a "special" matrix object created by makeCacheMatrix()
## and cache the result. On further calls return the cached result without 
## re-calcualting

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
