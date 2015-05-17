## Overall, This two functions create a special "matrix" object that can cache 
## its inverse. If the inverse has already been calculated, 
## then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
              x <<- y
              m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list( set=set,get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        k <- x$getinverse()
        if(!is.null(k)) {
                message("getting cached data")
                return(k)
        }
        data <- x$get()
        k <- solve(data, ...)
        x$setinverse(k)
        k
}
