## makeCacheMatrix creates a special matrix object
## cacheSolve calculates the inverse of the matrix

## makeCacheMatrix finds the inverse in the cache and returns if it's already calculated

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinv <- function(inv) inv_x <<-inv
        getinv <- function() inv_x
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


## CacheSolve returns the inverse of the matrix created with makeCacheMatrix
## cacheSolve retrieves if the inverse is already calculated, else recalculates.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinv()
        if (!is.null(inv_x)) {
                message("retrieving cached inverse")
                return(inv_x)
        } else {
                inv_x <- solve(x$get())
                x$setinv(inv_x)
                return(inv_x)
        }
}
