## Programming Assignment 2: Lexical Scoping
# Coded by Juan Eduardo Afanador

## General Description
#Matrix inversion is usually a costly computation and there may be some benefit to 
# caching the inverse of a matrix rather than compute it repeatedly. 
# The following functions cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
# Attributes: x = must be an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
    Inv <- NULL
    set <- function(y) {
        x <<- y
        Inv <<- NULL
    }
    get <- function() x
    setInverse <- function(Inverse) Inv <<- Inverse
    getInverse <- function() Inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve retrieves the inverse from the cache.
# Attributes: x = must be a "special matrix" of the type created by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    matr <- x$get()
    inverted <- solve(matr)
    x$setInverse(inverted)
    inverted
}
