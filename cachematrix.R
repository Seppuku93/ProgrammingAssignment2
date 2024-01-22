## Function makeCacheMatrix creates a matrix and saves the inverse,
## while the function cacheSolve checks if the inverse is already in the cache and calls the matrix.
## if the inverse is "new", it gets saved in the cache.

##additionally, x has a default matrix in case makeCacheMatrix() is entered 

makeCacheMatrix <- function(x = matrix(c(rep(c(1,0,0,0,0,0),4),1),5,5))) {
i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## cacheSolve calculates the inverse or sends a message in case the inverse is already contained in the chache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
            message("getting inversed matrix")
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
