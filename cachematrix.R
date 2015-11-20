###  cachematrix.R
### the project for R program
###  This R script is to write an R function which was able to
###  caching the inverse of a matrix. 
### 
### A pair of functions here.
### 
### First, makecachematrix creates a special "matrix" object 
### that can cache its inverse. As shown in the instruction
### of the assignment, this function includes a list containing
### a function to: 1.set the value of the matrix, 2.get the
### value of the matrix, 3.set the value of the inverse, and
### 4.get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

##Then, cachesolve computes the inverse of the special "matrix"
## returned by makecachematrix above. If the inverse has already 
##been calculated (and the matrix has not changed), then the 
##cachesolve should retrieve the inverse from the cache.

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
