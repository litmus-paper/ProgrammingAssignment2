## Programming Assignment 2 | R Programming | coursera
## By Nir Eden
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix.
## Below are pair of functions that cache the inverse of a matrix if possible or calculating it if needed.

# usage example:
# > mat<-matrix(c(4,3,3,2),2,2)
# > m<-makeCacheMatrix(mat)
# > cacheSolve(m)
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4
# > cacheSolve(m)
# getting cached data
# [,1] [,2]
# [1,]   -2    3
# [2,]    3   -4

## This function creates a special "matrix" object that can cache its inverse.
## Assumed that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(mean) m <<- mean
    getinv <- function() m
    
    ## Return a list
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    
    ## Return a matrix that is the inverse of 'x'
    m
}
