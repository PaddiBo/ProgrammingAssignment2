## This program is calculating the inverse of a matrix 
## which needs to be square invertible
## and employs caching to expedite the computation

## to check the computation the inverse of the original matrix 
## multiplied by the original matrix should give the unity matrix, i.e.
## cacheSolve(makeCacheMatrix(x)) %*% x -> unity matrix

## This function creates a special "matrix" list object that caches its inverse if available

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y) {
            x <<- y
            invm <<- NULL
        }
        get <- function() x
        setinv <- function(inv) invm <<- inv
        getinv <- function() invm
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    invm <- x$getinv()
    if(!is.null(invm)) {
        message("getting cached data")
        return(invm)
    }
    data <- x$get()
    invm <- solve(data, ...)
    x$setinv(invm)
    invm
}
