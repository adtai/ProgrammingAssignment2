## Put comments here that give an overall description of what your
## functions do
## Matrix inverseion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly (there are also alternatives to matrix inversion that we will
## not discuss here). Your assignment is to write a pair of functions that
## cache the inverse of a matrix.

## Write a short comment describing this function
## makeCacheMatrix: This function creates a special "matrix" object that can
##                  cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Initialize the inverse value as NULL
    inv <- NULL

    # Set the matrix 

    # Set the matrix
    set <- function(y) {
        x <<- y
        # NOTE - everytime the matrix is set, we'll reset the inverse
        # to NULL. Since we do that, every time we check the inverse,
        # it will only be cached if the matrix hasn't been changed.
        inv <<- NULL
    }

    # Get the matrix
    get <- function() x

    # Set the inverse of the matrix
    setinv <- function(inverse) inv <<- inverse

    # Get the inverse of the matrix
    getinv <- function() inv

    # Return the cacheable matrix methods?
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## cacheSolve: This function computes the inverse of the special "matrix"
##             returned by makeCacheMatrix above. If the inverse has already
##             been calculated (and the matrix has not changed), then the
##             cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # Check if the inverse has already been calculated 
    inv <- x$getinv()
    if (!is.null(inv)) {
        # If it has, return the cached value.
        message("getting cached data")
        # NOTE - we'll only have an inverse value if the matrix has not
        # changed. See above note for makeCacheMatrix$set
        return(inv)
    }
    # The inverse is not already cached
    # For this assignment, assume that the matrix is always invertible.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}

# Demo code:
# rows <- 1000
# f <- makeCacheMatrix(matrix(rnorm(rows*rows), nrow=rows, ncol=rows))
# cacheSolve(f)
# cacheSolve(f)
