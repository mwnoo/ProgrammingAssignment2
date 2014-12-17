## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# USAGE: a = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
# a$get()           Returns original matrix
# a$set()           Modify existing matirx
# cacheSolve(a)     Computes, cashes and returns the inverse of matrix a
# a$getinverse()    Returns the inverse of a
# a$setinverse()    Used by cacheSolve()
#
# cacheSolve(a)     Checks whether the inverse is already computed or not
#                   If not, the inverse is computed else the cashed inverse is returned
  
    m <- NULL
    # Update variable m
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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Check whether the inverse is already computed or not
    m <- x$getinverse()
    # If so, return the cashed inverse
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # If not, compute the inverse and store the result
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
