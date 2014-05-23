## The makeCacheMatrix function creates a special "matrix" object, then cacheSolve will
## calculate the inverse of the matrix.

## If the matrix inverse has already been calculated, it will find it in the cache, 
## return it, and not calculate it.

makeCacheMatrix <- function(x = matrix()) { 
        
        inverse_x <- NULL
        set <- function(y) {
                x <<- y
                inverse_x <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inverse_x <<-inverse
        getinverse <- function() inverse_x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## The cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function above.  

## If the cached inverse has already been calculated, then the cachesolve function retrieves the cached inverse,
## if not, it will compute, cache and return it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        inverse_x <- x$getinverse()
        if (!is.null(inverse_x)) {
                message("getting cached inverse matrix")
                return(inverse_x)
        } else {
                data <- solve(x$get())
                inverse_x <- mean(data, ...)
                x$setinverse(inverse_x)
                return(inverse_x)
        }
}
