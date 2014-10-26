## This function sets the value of matrix x, then gets the value.
## It also sets the value of the inverse of x, inv, and also gets the value.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function checks if the value of inv is already set. If so, 
## it retuns that vallue.
## Otherwise, the function calculates inv and sets this value in
## the cache function above.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
        ## Return a matrix that is the inverse of 'x'
}
