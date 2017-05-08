## The first function creates a special matrix and the second function
## creates the inverse of the special matrix created by first function
## if it is not already in the cache or retrive the inverted matrix 
## from the cache if it is already there.

## This function creates a list of 4 functions for creating special matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function retrives the inverted matrix of the matrix
## that was created by first function from the cache and if 
## it can not find it will claculate and save it in cache
## for later retrival

cacheSolve <- function(x, ...) {
		i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        ## Return a matrix that is the inverse of 'x'
		i
}
