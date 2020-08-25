## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "vector", 
##which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of the matrix
## get the value of the inverse f the matrix

## setmat function to set or initialize the matrix 'x'
## getmat function to get or retrieve the matrix values
## setinvmat function caches the inverse of the matrix
## getinvmat function retrieves the values of the inverse matrix in the cache
## 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    setmat <- function(y) {
        x <<- y
        m <<- NULL
    }
    getmat <- function() x
    setinvmat <- function(invmat) m <<- invmat
    getinvmat <- function() m
    list(setmat = setmat, getmat = getmat,
         setinvmat = setinvmat,
         getinvmat = getinvmat)
}


## Write a short comment describing this function

## cachesolve function returns a matrix that is the inverse
## of its parameter x

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinvmat()
    if(!is.null(m)) {
        message("getting the inverse of the matrix")
        return(m)
    }
    data <- x$getmat()
    m <- solve(data, ...)
    x$setinvmat(m)
    m
}
