# Matrix inversion can be very costly computation and there may be some benefit
# to cache the inverse of a matrix rather than computing it every time. The
# following two functions are used to cache the inverse of a matrix using lexical scoping in R.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                inv <<- NULL
                x <<- y
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


# The following function returns the inverse of the matrix. It first checks if  
# the inverse has already been computed. If so, it gets the result from cache 
# If not, it computes the inverse, sets the value in the cache using 
# setinverse function.

# This function assumes that the matrix is always invertible.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse();
        if(!is.null(inv)){
                print("Getting inverse from the cache.")
                return(inv)
        }
        print("Calucating inverse for the first time.")
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
