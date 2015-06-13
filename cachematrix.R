## This script contains two function. 
## makeCacheMatrix function creates a "matrix" object that can cache its inverse. 
## cacheSolve function computes the inverse of the "matrix" returned by makeCacheMatrix.

# makeCacheMatrix creates a list that contains four function: set, get, getinv and setinv.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function (y = matrix()){
                # set the values of the matrix
                x <<- y
                inv <<- NULL
        }
        get <- function() x  # get the matrix
        setinv <- function(solve) inv <<- solve  # set the values of inverse matrix
        getinv <- function() inv   # get the cached values of inverse matrix
        list (set = set, get = get, 
              setinv = setinv, getinv = getinv) # return a function list

}


## cacheSolve checks to see if the matrix inverse has already been calculated. If so, 
## it gets the inverse matrix from the cache and skips the computation.  Otherwise, 
## it calculates the matrix inverse and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                # Verify the inv exists and is not NULL
                message("getting cached matrix inverse")
                return(inv) # Return a matrix that is the inverse of 'x'
                
        }
        matrix <- x$get()
        inv <- solve(matrix) # inv does not exist, compute inverse
        x$setinv(inv)        # store the inverse 
        inv
}
