
## cache/retrieve matrix. returns special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Return a matrix that is the inverse of special "matrix" object returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## look at cache for given matrix
        m <- x$getinverse()
	## if matrix is cached - return inverse from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
	## otherwise call solve function to perform inversion
        data <- x$get()
        m <- solve(data, ...)
	## save result in cache
        x$setinverse(m)
}
