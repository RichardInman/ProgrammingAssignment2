## Programming Assignment 2
## Two functions are presented: (1) makeCacheMatrix which creates a special matrix
## object, and (2) cacheSolve which computes the inverse of the special matrix.


## This function creates a special "matrix" object that can cache its inverse
## Returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # initialize value of m as null
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x # returns matrix x 
        setInverse <- function(inverse) m <<- inverse # calculates inverse of matrix
        getInverse <- function() m # get inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by 
##`makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        # x is output of makeCacheMatrix
        m <- x$getInverse
        
        #determine if null already calculated if so present data
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
                
        }
        #otherwise calculate inverse
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
        
}
