## The following functions makeCacheMatrix and cacheSolve will together utilize lexical scoping of
## mechanism of R in order to store an evaluated inverse of a given solvable matrix, so that the
## inverse can be retrieved without having to go through calculation which can be costly.

## makeCacheMatrix function will store in its environment the inverse of the given matrix. The
## created object from makeCacheMatrix will contain the following functions in order to assign and
## retrieve the matrix and its inverse: set(), get(), setinverse() and getinverse().

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(calc) inv <<- calc
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Given an instance of makeCacheMatrix function, cacheSolve utilizes the getter and setter
## functions in order to calculate and store the inverse of the matrix set in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mtx <- x$get()
    inv <- solve(mtx)
    x$setinverse(inv)
    inv
}
