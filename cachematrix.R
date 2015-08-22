# A pair of functions to wrap a matrix with a facility to cache its inverse.
# Example usage:

#   orig = matrix(rnorm(25),nrow=5,ncol=5)  # Create a random 5x5 matrix
#   cmatrix = makeCacheMatrix(orig)         # Wrap with inverse cache
#   inverse = cacheSolve(cmatrix)           # Get the inverse
#   cmatrix$get() %*% inverse               # Check result
#   i2 = cacheSolve(cmatrix)                # Should say "getting cached data"

# makeCacheMatrix() creates a list of get/set/getinverse/setinverse functions
# that operate identically to the makeVector example given in the assignment:
# getting and setting a matrix value and its cached inverse value,
# respectively.  Setting the matrix value clears any cached inverse value.  The
# functions execute in a closure containing the original matrix and (if
# present) its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
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


# As with the cachemean example, here we calculate and return a matrix's
# inverse; if we calculated it before, then we return the remembered inverse
# instead of recalculating.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}
