## This creates a matirx that will cache the inverse to increase processing as it will 
## only re-calculate the inverse if the matrix is changed.

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) iv <<- inv
        getinverse <- function() iv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function is the caching area, it determines if the inverse is needed and calcs it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        iv <- x$getinverse()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        data <- x$get()
        # Create the inverse here
        iv <- solve(data, ...)
        
        # update the inverse variable
        x$setinverse(iv)
        iv
}
