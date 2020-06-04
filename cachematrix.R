## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## this function creates a special vector containing
## four functions that respectively set the cached value
## for the matrix, get the cached value, set the cached inverse
## for the matrix and get the cached inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # set cached matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # get cached matrix
    get <- function() x
    # set the inverse cache
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    # get the cached inverse
    list(set = set, get = get,
         getinv = getinv, setinv = setinv)
}


## Write a short comment describing this function
## This function returns the inverse of a matrix
## given its special vector. If cache for the inverse
## is set, then simply return the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <-  x$getinv()
    # cache for the inverse found
    if (!is.null(inv)) {
        message("Getting matrix inverse from cache...")
        return(inv)
    }
    # cache not found - compute inverse and store it in cache
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv
}
