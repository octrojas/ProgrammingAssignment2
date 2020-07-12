## The pair of functions presented here are able to cache the inverse 
of a matrix,  optimizing the work if this inverse matrix is requested later.

## The first function, makeCacheMatrix creates a special "matrix", 
## that can cache its inverse.

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


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
##(and the matrix has not changed), cacheSolve should retrieve the inverse 
## from the cache.

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
        ## Return a matrix that is the inverse of 'x'
}

#Example
matrix <- matrix(c(4,8,7,4,2,1,2,3,1), nrow=3, ncol=3)
z <- makeCacheMatrix (matrix)
cacheSolve (z)

