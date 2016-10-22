
## Inspired in the provided example, "makeCacheMatrix" creates a special object containing the original matrix data and a set of functions to support the cache feature 

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


## Inspired in the provided example, "cacheSolve" computes the inverse of the "special matrix" (built by means of the "makeCacheMatrix" function)

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinverse()
     if(!is.null(m)) {
         message("Getting cached data.")
         return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setinverse(m)
     m
}
