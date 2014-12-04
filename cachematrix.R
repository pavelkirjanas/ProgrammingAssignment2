## The following pair of functions caches the inverse of a matrix.

##

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL 
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        set.inverse <- function(solution) inverse <<- solution
        get.inverse <- function() inverse
        list(set = set,
             get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}

## This function computes the inverse of the special "matrix" 
## returned by `makeCacheMatrix` above. If the inverse has 
## already been calculated (and the matrix has not changed),
## then `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse <- x$get.inverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$set.inverse(inverse)
        inverse        
}
