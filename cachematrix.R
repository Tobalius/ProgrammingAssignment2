## Functions for cached calculation of matrix inverses. First one creates a
## "matrix object" that stores a matrix, its inverse(init to null), setter 
## and getter methods for both. Second one gives the inverse of the matrix
## stored in a "matrix object", if NULL, calculates it and updates the object


## Takes a matrix. Returns a list of setter and getter functions (matrix and
## inverse are stored in their closures)

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
            x <<- y
            xinv <<- NULL
        }
        get <- function() x
        setinv <- function(y) xinv <<- y
        getinv <- function() xinv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## Takes a "matrix object". Returns the inverse of the associated matrix.
## Checks cache first, updates object as needed.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
            message("cached data")
            return(inv)
        }
        x$setinv(solve(x$get()))
        x$getinv()
}
