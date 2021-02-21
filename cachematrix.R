## The following functions create a special object that stores a numeric 
## vector and caches its mean.

## The first function creates a special vector containing a function that sets the 
## value of the vector, gets the value of the vector, sets the value of the 
## mean, and gets the value of the mean.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The second function calculates the mean of the special vector created with
## the above function. If the mean has already been calculated it gets it from 
## the cache. If the mean has not been calculated it calculates the mean and 
## sets the value of the mean in the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
} 

